#include <math.h>
#include <float.h>
#include <cuda.h>

__global__ void gpu_Heat (float *h, float *g, int N) {

	// TODO: kernel computation
	//...

	int x = blockIdx.x * blockDim.x + threadIdx.x;
	int y = blockIdx.y * blockDim.y + threadIdx.y;

	if(x <= 0 || x >= N - 1 || y <= 0 || y >= N - 1)
	    return;

	g[N * x + y] =  0.25 * (f[N * x       + y - 1] + 
				f[N * x       + y + 1] + 
				f[N * (x - 1) + y    ] + 
				f[N * (x + 1) + y    ]);
}
