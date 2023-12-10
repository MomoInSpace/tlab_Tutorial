program gpu_detection
    use cudafor
    implicit none
    
    integer :: deviceCount
    
    call cudaGetDeviceCount(deviceCount)
    if (deviceCount > 0) then
        print *, 'Number of GPUs detected:', deviceCount
    else
        print *, 'No GPUs detected.'
    end if
    
end program gpu_detection
