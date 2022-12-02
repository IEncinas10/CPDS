NVCC        = nvcc
#NVCC_FLAGS  = -g -Xcompiler -rdynamic -lineinfo
NVCC_FLAGS  = -O3 -Wno-deprecated-gpu-targets
LD_FLAGS    = #-lcudart -Xlinker

ALL	= heat-CUDA
all: $(ALL)

kernels.o: kernels.cu
	$(NVCC) -c $(NVCC_FLAGS) $+ $(LD_FLAGS) -o $@

heat-CUDA: heat-CUDA.cu kernels.o
	$(NVCC) $(NVCC_FLAGS) $+ $(LD_FLAGS) -o $@

clean:
	rm -fr $(ALL) *.o *~ *.ppm *.ps *.txt sub*.e* sub*.o* *.prv *.pcf *.row TRACE.mpits set-0 *.sym

