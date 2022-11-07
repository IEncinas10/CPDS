#include <math.h>
#include <float.h>
#include <cuda.h>

__global__ void gpu_Heat (float *h, float *g, int N) {

	// TODO: kernel computation
	//...

	int x = blockIdx.x * blockDim.x + threadIdx.x;
	int y = blockIdx.y * blockDim.y + threadIdx.y;

	if(x < 1 || x > N - 2 || y < 1 || y > N - 2)
	    return;

	g[N * x + y] =  0.25 * (h[N * x       + y - 1] + 
				h[N * x       + y + 1] + 
				h[N * (x - 1) + y    ] + 
				h[N * (x + 1) + y    ]);
}
