#include <cuda.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

typedef struct {
    float posx;
    float posy;
    float range;
    float temp;
} heatsrc_t;

typedef struct {
    unsigned maxiter;	 // maximum number of iterations
    unsigned resolution; // spatial resolution
    int algorithm;	 // 0=>Jacobi, 1=>Gauss

    unsigned visres; // visualization resolution

    float *u, *uhelp;
    float *uvis;

    unsigned numsrcs; // number of heat sources
    heatsrc_t *heatsrcs;
} algoparam_t;

// function declarations
int read_input(FILE *infile, algoparam_t *param);
void print_params(algoparam_t *param);
int initialize(algoparam_t *param);
int finalize(algoparam_t *param);
void write_image(FILE *f, float *u, unsigned sizex, unsigned sizey);
int coarsen(float *uold, unsigned oldx, unsigned oldy, float *unew, unsigned newx, unsigned newy);

__global__ void gpu_Heat(float *h, float *g, int N);
__global__ void gpu_Heat_diff(float *h, float *g, float *diff, int N);
__global__ void reduce(float *idata, float *odata, int N);
__global__ void Kernel06(float *g_idata, float *g_odata);

#define NB 8
#define min(a, b) (((a) < (b)) ? (a) : (b))

float cpu_residual(float *u, float *utmp, unsigned sizex, unsigned sizey,
		   float *residual_computation_time) {
    float diff, sum = 0.0;

    clock_t start = clock();

    for (int i = 1; i < sizex - 1; i++)
	for (int j = 1; j < sizey - 1; j++) {
	    diff = utmp[i * sizey + j] - u[i * sizey + j];
	    sum += diff * diff;
	}
    clock_t end = clock();

    const float seconds = (float)(end - start) / CLOCKS_PER_SEC;
    *residual_computation_time += seconds;

    return (sum);
}

float cpu_jacobi(float *u, float *utmp, unsigned sizex, unsigned sizey) {
    float diff, sum = 0.0;
    int nbx, bx, nby, by;

    nbx = NB;
    bx = sizex / nbx;
    nby = NB;
    by = sizey / nby;
    for (int ii = 0; ii < nbx; ii++)
	for (int jj = 0; jj < nby; jj++)
	    for (int i = 1 + ii * bx; i <= min((ii + 1) * bx, sizex - 2); i++)
		for (int j = 1 + jj * by; j <= min((jj + 1) * by, sizey - 2); j++) {
		    utmp[i * sizey + j] = 0.25 * (u[i * sizey + (j - 1)] + // left
						  u[i * sizey + (j + 1)] + // right
						  u[(i - 1) * sizey + j] + // top
						  u[(i + 1) * sizey + j]); // bottom
		    diff = utmp[i * sizey + j] - u[i * sizey + j];
		    sum += diff * diff;
		}
    return (sum);
}

void usage(char *s) {
    fprintf(stderr, "Usage: %s <input file> -t threads -b blocks\n", s);
    fprintf(stderr, "       -t number of threads per block in each dimension (e.g. 16)\n");
}

int main(int argc, char *argv[]) {
    unsigned iter;
    FILE *infile, *resfile;
    char *resfilename;

    // algorithmic parameters
    algoparam_t param;
    int np;

    // check arguments
    if (argc < 4) {
	usage(argv[0]);
	return 1;
    }

    // check input file
    if (!(infile = fopen(argv[1], "r"))) {
	fprintf(stderr, "\nError: Cannot open \"%s\" for reading.\n\n", argv[1]);

	usage(argv[0]);
	return 1;
    }

    // check result file
    resfilename = "heat.ppm";

    if (!(resfile = fopen(resfilename, "w"))) {
	fprintf(stderr, "\nError: Cannot open \"%s\" for writing.\n\n", resfilename);
	usage(argv[0]);
	return 1;
    }

    // check input
    if (!read_input(infile, &param)) {
	fprintf(stderr, "\nError: Error parsing input file.\n\n");
	usage(argv[0]);
	return 1;
    }

    // full size (param.resolution are only the inner points)
    np = param.resolution + 2;
    bool gpu_reduction = false;

    int Grid_Dim, Block_Dim; // Grid and Block structure values
    if (strcmp(argv[2], "-t") == 0) {
	Block_Dim = atoi(argv[3]);
	Grid_Dim = np / Block_Dim + ((np % Block_Dim) != 0);
	;
	if ((Block_Dim * Block_Dim) > 512) {
	    printf("Error -- too many threads in block, try again\n");
	    return 1;
	}

	if (argc > 4 && strcmp(argv[4], "gpu") == 0) {
	    printf("Reduction to be performed on the GPU\n");
	    gpu_reduction = true;
	}
    } else {
	fprintf(stderr, "Usage: %s <input file> -t threads -b blocks [gpu]\n", argv[0]);
	fprintf(stderr, "       -t number of threads per block in each dimension (e.g. 16)\n");
	return 0;
    }

    fprintf(stderr, "\nSolving Heat equation on the CPU and the GPU\n");
    fprintf(stderr, "--------------------------------------------\n");
    print_params(&param);

    fprintf(stdout, "\nExecution on CPU (sequential)\n-----------------------------\n");
    if (!initialize(&param)) {
	fprintf(stderr, "Error in Solver initialization.\n\n");
	return 1;
    }

    // starting time
    float elapsed_time_ms;   // which is applicable for asynchronous code also
    cudaEvent_t start, stop; // using cuda events to measure time
    cudaEventCreate(&start); // instrument code to measure start time
    cudaEventCreate(&stop);

    cudaEventRecord(start, 0);
    cudaEventSynchronize(start);

    iter = 0;
    float residual;
    while (1) {
	residual = cpu_jacobi(param.u, param.uhelp, np, np);
	float *tmp = param.u;
	param.u = param.uhelp;
	param.uhelp = tmp;

	iter++;

	// solution good enough ?
	if (residual < 0.00005)
	    break;

	// max. iteration reached ? (no limit with maxiter=0)
	if (iter >= param.maxiter)
	    break;
    }

    cudaEventRecord(stop, 0); // instrument code to measue end time
    cudaEventSynchronize(stop);
    cudaEventElapsedTime(&elapsed_time_ms, start, stop);

    // Flop count after iter iterations
    float flop = iter * 11.0 * param.resolution * param.resolution;

    fprintf(stdout, "Time on CPU in ms.= %f ", elapsed_time_ms);
    fprintf(stdout, "(%3.3f GFlop => %6.2f MFlop/s)\n", flop / 1000000000.0,
	    flop / elapsed_time_ms / 1000);
    fprintf(stdout, "Convergence to residual=%f: %d iterations\n", residual, iter);

    finalize(&param);

    fprintf(stdout, "\nExecution on GPU\n----------------\n");
    fprintf(stderr, "Number of threads per block in each dimension = %d\n", Block_Dim);
    fprintf(stderr, "Number of blocks per grid in each dimension   = %d\n", Grid_Dim);

    if (!initialize(&param)) {
	fprintf(stderr, "Error in Solver initialization.\n\n");
	return 1;
    }

    dim3 Grid(Grid_Dim, Grid_Dim);
    dim3 Block(Block_Dim, Block_Dim);

    // starting time
    cudaEventRecord(start, 0);
    cudaEventSynchronize(start);

    float *dev_u, *dev_uhelp, *dev_diff, *dev_block_red, *dev_gpu_red;

    // TODO: Allocation on GPU for matrices u and uhelp
    //...

    cudaMalloc((void **)&dev_u, np * np * sizeof(float));
    cudaMalloc((void **)&dev_uhelp, np * np * sizeof(float));
    cudaMalloc((void **)&dev_diff, (np - 2) * (np - 2) * sizeof(float));

    int elems_per_thread = 4;
    int num_threads_reduce = 128;
    int num_blocks_reduce = (np - 2) * (np - 2) / (elems_per_thread * num_threads_reduce);
    fprintf(stdout, "\nGPU reduction (%d): %d elements per thread, %d blocks, %d threads\n\n", (np - 2) * (np - 2), elems_per_thread, num_blocks_reduce, num_threads_reduce);
    cudaMalloc((void **)&dev_block_red, num_blocks_reduce * sizeof(float));
    cudaMemset(dev_block_red, 0, num_blocks_reduce*sizeof(float));

    cudaMalloc((void **)&dev_gpu_red, sizeof(float));

    // cudamalloc dev_u    , sizeof(float) * np * np
    // cudamalloc dev_uhelp, sizeof(float) * np * np
    //

    // TODO: Copy initial values in u and uhelp from host to GPU
    //...
    // cudaMemcpy param.u     -> dev_u    , hosttodevice
    // cudaMemcpy param.uhelp -> dev_uhelp, hosttodevice

    cudaMemcpy(dev_u, param.u, np * np * sizeof(float), cudaMemcpyHostToDevice);
    cudaMemcpy(dev_uhelp, param.uhelp, np * np * sizeof(float), cudaMemcpyHostToDevice);

    // param.u = p
    // param.uhelp
    //

    iter = 0;
    float residual_cpu_time = 0, gpu_red;
    while (1) {

	if (gpu_reduction) {
	    // v2
	    gpu_Heat_diff<<<Grid, Block>>>(dev_u, dev_uhelp, dev_diff, np);
	    reduce<<<num_blocks_reduce, num_threads_reduce>>>(dev_diff, dev_block_red, (np - 2) * (np - 2));
	    Kernel06<<<1, num_blocks_reduce/2>>>(dev_block_red, dev_gpu_red);
	    cudaMemcpy(&gpu_red, dev_gpu_red, sizeof(float), cudaMemcpyDeviceToHost);
	    residual = gpu_red;
	    // end v2
	} else {
	    // v1

	    gpu_Heat<<<Grid, Block>>>(dev_u, dev_uhelp, np);
	    cudaDeviceSynchronize(); // Wait for compute device to finish.

	    // end v1

	    // TODO: residual is computed on host, we need to get from GPU values computed in u and
	    // uhelp
	    //...

	    // cudaMemcpy dev_u     -> param.u    , devicetohost
	    // cudaMemcpy dev_uhelp -> param.uhelp, devicetohost
	    cudaMemcpy(param.u, dev_u, np * np * sizeof(float), cudaMemcpyDeviceToHost);
	    cudaMemcpy(param.uhelp, dev_uhelp, np * np * sizeof(float), cudaMemcpyDeviceToHost);

	    //

	    residual = cpu_residual(param.u, param.uhelp, np, np, &residual_cpu_time);
	}

	float *tmp = dev_u;
	dev_u = dev_uhelp;
	dev_uhelp = tmp;

	iter++;

	// solution good enough ?
	if (residual < 0.00005)
	    break;

	// max. iteration reached ? (no limit with maxiter=0)
	if (iter >= param.maxiter)
	    break;
    }

    // TODO: get result matrix from GPU
    //...
    // cudaMemcpy dev_uhelp     -> param.u    , devicetohost
    //
    cudaMemcpy(param.u, dev_u, np * np * sizeof(float), cudaMemcpyDeviceToHost);

    // TODO: free memory used in GPU
    //...
    // cudafree dev_u, dev_uhelp
    cudaFree(dev_u);
    cudaFree(dev_uhelp);
    cudaFree(dev_diff);
    cudaFree(dev_gpu_red);
    //

    cudaEventRecord(stop, 0); // instrument code to measue end time
    cudaEventSynchronize(stop);
    cudaEventElapsedTime(&elapsed_time_ms, start, stop);

    fprintf(stdout, "\nTime on GPU in ms. = %f\n", elapsed_time_ms);
    fprintf(stdout, "Jacobi on GPU in ms. = %f (+ mem coyping)\n",
	    elapsed_time_ms - residual_cpu_time * 1000);
    fprintf(stdout, "Residual computation in CPU: %f ms\n", residual_cpu_time * 1000);
    fprintf(stdout, "(%3.3f GFlop => %6.2f MFlop/s)\n", flop / 1000000000.0,
	    flop / elapsed_time_ms / 1000);
    fprintf(stdout, "Convergence to residual=%f: %d iterations\n", residual, iter);

    cudaEventDestroy(start);
    cudaEventDestroy(stop);

    // for plot...
    coarsen(param.u, np, np, param.uvis, param.visres + 2, param.visres + 2);

    write_image(resfile, param.uvis, param.visres + 2, param.visres + 2);

    finalize(&param);

    return 0;
}

/*
 * Initialize the iterative solver
 * - allocate memory for matrices
 * - set boundary conditions according to configuration
 */
int initialize(algoparam_t *param)

{
    int i, j;
    float dist;

    // total number of points (including border)
    const int np = param->resolution + 2;

    //
    // allocate memory
    //
    (param->u) = (float *)calloc(sizeof(float), np * np);
    (param->uhelp) = (float *)calloc(sizeof(float), np * np);
    (param->uvis) = (float *)calloc(sizeof(float), (param->visres + 2) * (param->visres + 2));

    if (!(param->u) || !(param->uhelp) || !(param->uvis)) {
	fprintf(stderr, "Error: Cannot allocate memory\n");
	return 0;
    }

    for (i = 0; i < param->numsrcs; i++) {
	/* top row */
	for (j = 0; j < np; j++) {
	    dist = sqrt(pow((float)j / (float)(np - 1) - param->heatsrcs[i].posx, 2) +
			pow(param->heatsrcs[i].posy, 2));

	    if (dist <= param->heatsrcs[i].range) {
		(param->u)[j] += (param->heatsrcs[i].range - dist) / param->heatsrcs[i].range *
				 param->heatsrcs[i].temp;
	    }
	}

	/* bottom row */
	for (j = 0; j < np; j++) {
	    dist = sqrt(pow((float)j / (float)(np - 1) - param->heatsrcs[i].posx, 2) +
			pow(1 - param->heatsrcs[i].posy, 2));

	    if (dist <= param->heatsrcs[i].range) {
		(param->u)[(np - 1) * np + j] += (param->heatsrcs[i].range - dist) /
						 param->heatsrcs[i].range * param->heatsrcs[i].temp;
	    }
	}

	/* leftmost column */
	for (j = 1; j < np - 1; j++) {
	    dist = sqrt(pow(param->heatsrcs[i].posx, 2) +
			pow((float)j / (float)(np - 1) - param->heatsrcs[i].posy, 2));

	    if (dist <= param->heatsrcs[i].range) {
		(param->u)[j * np] += (param->heatsrcs[i].range - dist) / param->heatsrcs[i].range *
				      param->heatsrcs[i].temp;
	    }
	}

	/* rightmost column */
	for (j = 1; j < np - 1; j++) {
	    dist = sqrt(pow(1 - param->heatsrcs[i].posx, 2) +
			pow((float)j / (float)(np - 1) - param->heatsrcs[i].posy, 2));

	    if (dist <= param->heatsrcs[i].range) {
		(param->u)[j * np + (np - 1)] += (param->heatsrcs[i].range - dist) /
						 param->heatsrcs[i].range * param->heatsrcs[i].temp;
	    }
	}
    }

    // Copy u into uhelp
    float *putmp, *pu;
    pu = param->u;
    putmp = param->uhelp;
    for (j = 0; j < np; j++)
	for (i = 0; i < np; i++)
	    *putmp++ = *pu++;

    return 1;
}

/*
 * free used memory
 */
int finalize(algoparam_t *param) {
    if (param->u) {
	free(param->u);
	param->u = 0;
    }

    if (param->uhelp) {
	free(param->uhelp);
	param->uhelp = 0;
    }

    if (param->uvis) {
	free(param->uvis);
	param->uvis = 0;
    }

    return 1;
}

/*
 * write the given temperature u matrix to rgb values
 * and write the resulting image to file f
 */
void write_image(FILE *f, float *u, unsigned sizex, unsigned sizey) {
    // RGB table
    unsigned char r[1024], g[1024], b[1024];
    int i, j, k;

    float min, max;

    j = 1023;

    // prepare RGB table
    for (i = 0; i < 256; i++) {
	r[j] = 255;
	g[j] = i;
	b[j] = 0;
	j--;
    }
    for (i = 0; i < 256; i++) {
	r[j] = 255 - i;
	g[j] = 255;
	b[j] = 0;
	j--;
    }
    for (i = 0; i < 256; i++) {
	r[j] = 0;
	g[j] = 255;
	b[j] = i;
	j--;
    }
    for (i = 0; i < 256; i++) {
	r[j] = 0;
	g[j] = 255 - i;
	b[j] = 255;
	j--;
    }

    min = DBL_MAX;
    max = -DBL_MAX;

    // find minimum and maximum
    for (i = 0; i < sizey; i++) {
	for (j = 0; j < sizex; j++) {
	    if (u[i * sizex + j] > max)
		max = u[i * sizex + j];
	    if (u[i * sizex + j] < min)
		min = u[i * sizex + j];
	}
    }

    fprintf(f, "P3\n");
    fprintf(f, "%u %u\n", sizex, sizey);
    fprintf(f, "%u\n", 255);

    for (i = 0; i < sizey; i++) {
	for (j = 0; j < sizex; j++) {
	    k = (int)(1023.0 * (u[i * sizex + j] - min) / (max - min));
	    fprintf(f, "%d %d %d  ", r[k], g[k], b[k]);
	}
	fprintf(f, "\n");
    }
}

int coarsen(float *uold, unsigned oldx, unsigned oldy, float *unew, unsigned newx, unsigned newy) {
    int i, j;

    int stepx;
    int stepy;
    int stopx = newx;
    int stopy = newy;

    if (oldx > newx)
	stepx = oldx / newx;
    else {
	stepx = 1;
	stopx = oldx;
    }
    if (oldy > newy)
	stepy = oldy / newy;
    else {
	stepy = 1;
	stopy = oldy;
    }

    // NOTE: this only takes the top-left corner,
    // and doesnt' do any real coarsening
    for (i = 0; i < stopy - 1; i++) {
	for (j = 0; j < stopx - 1; j++) {
	    unew[i * newx + j] = uold[i * oldx * stepy + j * stepx];
	}
    }

    return 1;
}

#define BUFSIZE 100
int read_input(FILE *infile, algoparam_t *param) {
    int i, n;
    char buf[BUFSIZE];

    fgets(buf, BUFSIZE, infile);
    n = sscanf(buf, "%u", &(param->maxiter));
    if (n != 1)
	return 0;

    fgets(buf, BUFSIZE, infile);
    n = sscanf(buf, "%u", &(param->resolution));
    if (n != 1)
	return 0;

    param->visres = param->resolution;

    fgets(buf, BUFSIZE, infile);
    n = sscanf(buf, "%u", &(param->numsrcs));
    if (n != 1)
	return 0;

    (param->heatsrcs) = (heatsrc_t *)malloc(sizeof(heatsrc_t) * (param->numsrcs));

    for (i = 0; i < param->numsrcs; i++) {
	fgets(buf, BUFSIZE, infile);
	n = sscanf(buf, "%f %f %f %f", &(param->heatsrcs[i].posx), &(param->heatsrcs[i].posy),
		   &(param->heatsrcs[i].range), &(param->heatsrcs[i].temp));

	if (n != 4)
	    return 0;
    }

    return 1;
}

void print_params(algoparam_t *param) {
    int i;

    fprintf(stdout, "Iterations        : %u\n", param->maxiter);
    fprintf(stdout, "Resolution        : %u\n", param->resolution);
    fprintf(stdout, "Num. Heat sources : %u\n", param->numsrcs);

    for (i = 0; i < param->numsrcs; i++) {
	fprintf(stdout, "  %2d: (%2.2f, %2.2f) %2.2f %2.2f \n", i + 1, param->heatsrcs[i].posx,
		param->heatsrcs[i].posy, param->heatsrcs[i].range, param->heatsrcs[i].temp);
    }
}

