#include <cuda.h>
#include <float.h>
#include <math.h>

__global__ void gpu_Heat(float *h, float *g, int N) {

    int x = blockIdx.x * blockDim.x + threadIdx.x;
    int y = blockIdx.y * blockDim.y + threadIdx.y;

    if (x < 1 || x > N - 2 || y < 1 || y > N - 2)
	return;

    g[N * x + y] =
	0.25 * (h[N * x + y - 1] + h[N * x + y + 1] + h[N * (x - 1) + y] + h[N * (x + 1) + y]);
}

__global__ void gpu_Heat_diff(float *h, float *g, float *diff, int N) {

    int x = blockIdx.x * blockDim.x + threadIdx.x;
    int y = blockIdx.y * blockDim.y + threadIdx.y;

    if (x < 1 || x > N - 2 || y < 1 || y > N - 2)
	return;

    g[N * x + y] =
	0.25 * (h[N * x + y - 1] + h[N * x + y + 1] + h[N * (x - 1) + y] + h[N * (x + 1) + y]);

    diff[(N - 2) * (x - 1) + y - 1] = (g[N * x + y] - h[N * x + y]) * (g[N * x + y] - h[N * x + y]);
}


#define MAX_THREADS_PER_BLOCK 2048

// idata: array of N elements.
// odata: output array of "GridDim" elements, 1 per block
__global__ void reduce(float *idata, float *odata, int N) {
    __shared__ float sdata[MAX_THREADS_PER_BLOCK];
    unsigned int s;

    // Cada thread realiza la suma parcial de los datos que le
    // corresponden y la deja en la memoria compartida
    unsigned int tid = threadIdx.x;
    unsigned int i = blockIdx.x * (blockDim.x * 2) + threadIdx.x;
    unsigned int gridSize = blockDim.x * 2 * gridDim.x;
    sdata[tid] = 0;
    while (i < N) {
	sdata[tid] += idata[i] + idata[i + blockDim.x];
	i += gridSize;
    }
    __syncthreads();

    // Hacemos la reduccion en la memoria compartida
    for (s = blockDim.x / 2; s > 32; s >>= 1) {
	if (tid < s)
	    sdata[tid] += sdata[tid + s];
	__syncthreads();
    }
    // desenrrollamos el ultimo warp activo
    if (tid < 32) {
	volatile float *smem = sdata;

	smem[tid] += smem[tid + 32];
	smem[tid] += smem[tid + 16];
	smem[tid] += smem[tid + 8];
	smem[tid] += smem[tid + 4];
	smem[tid] += smem[tid + 2];
	smem[tid] += smem[tid + 1];
    }

    // El thread 0 escribe el resultado de este bloque en la memoria global
    if (tid == 0)
	odata[blockIdx.x] = sdata[0];
}

__global__ void Kernel06(float *g_idata, float *g_odata) {
  __shared__ float sdata[MAX_THREADS_PER_BLOCK];
  unsigned int s;

  // Cada thread carga 2 elementos desde la memoria global,
  // los suma y los deja en la memoria compartida
  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*(blockDim.x*2) + threadIdx.x;
  sdata[tid] = g_idata[i] + g_idata[i+blockDim.x];
  __syncthreads();

  // Hacemos la reduccion en la memoria compartida
  for (s=blockDim.x/2; s>32; s>>=1) {
    if (tid < s)
      sdata[tid] += sdata[tid + s];
    __syncthreads();
  }

 // desenrrollamos el ultimo warp activo
 if (tid < 32) {
   volatile float *smem = sdata;

   smem[tid] += smem[tid + 32];
   smem[tid] += smem[tid + 16];
   smem[tid] += smem[tid + 8];
   smem[tid] += smem[tid + 4];
   smem[tid] += smem[tid + 2];
   smem[tid] += smem[tid + 1];
 }

 // El thread 0 escribe el resultado de este bloque en la memoria global
 if (tid == 0) g_odata[blockIdx.x] = sdata[0];

}


