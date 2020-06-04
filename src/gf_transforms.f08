submodule (gf_fourier) gf_transforms

contains

  subroutine momspace_to_realspace(momentum, rlspace, ierr)
    type(greensfunc), intent(in) :: momentum(:, :, :)
    type(greensfunc), intent(out) :: rlspace(:, :)
    integer :: ierr

    !routine variables
    type(greensfunc), allocatable :: workreal(:, :, :, :, :, :)
    integer :: x_size, y_size, z_size, nomega
    integer :: npoints
    integer :: ierr1
    integer :: ix, iy, iz, jx, jy, jz, io

    x_size = size(momentum,1)
    y_size = size(momentum,2)
    z_size = size(momentum,3)
    nomega = size(momentum(1, 1, 1)%GF, 1)

    call error_check()

    allocate(workreal(x_size, y_size, z_size,x_size, y_size, z_size))
    call allocate_gf(workreal, nomega)
    call initialise_gf(workreal,cmplx_zero)
    
    copy_momspace_to_diagonal:do ix= 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             do io = 1, nomega
                workreal(ix, iy, iz, ix, iy, iz)%GF(io) = &
                     & momentum(ix, iy, iz)%GF(io)
             enddo
          enddo
       enddo
    enddo copy_momspace_to_diagonal

    call fft_on_kdash(workreal, backward_fft)
    call fft_on_k(workreal, forward_fft)
 
    !reshape array
    rlspace = reshape(workreal, [npoints, npoints])

  contains

    subroutine error_check()

      if (x_size*y_size*z_size.ne.size(rlspace,1)) then
         ierr = 1
      elseif (x_size*y_size*z_size.ne.size(rlspace,2)) then
         ierr = 1
      elseif (size(rlspace,1).ne.size(rlspace,2)) then
         ierr = 2
      else
         ierr = 0
      endif

    end subroutine error_check

  end subroutine momspace_to_realspace

  subroutine realspace_to_momspace(rlspace, momentum, ierr)
    type(greensfunc), intent(out) :: momentum(:, :, :)
    type(greensfunc), intent(in) :: rlspace(:, :)
    integer :: ierr

    !routine variables
    type(greensfunc), allocatable :: workreal(:, :, :, :, :, :)
    integer :: x_size, y_size, z_size, nomega
    integer :: npoints
    integer :: ierr1
    integer :: ix, iy, iz, jx, jy, jz, io

    x_size = size(momentum,1)
    y_size = size(momentum,2)
    z_size = size(momentum,3)
    nomega = size(momentum(1, 1, 1)%GF, 1)
    call error_check()

    allocate(workreal(x_size, y_size, z_size,x_size, y_size, z_size))
    call allocate_gf(workreal, nomega)

    workreal = reshape(rlspace, [x_size, y_size, z_size, x_size,&
         & y_size, z_size])

    call fft_on_k(workreal,forward_fft)

    call fft_on_kdash(workreal,backward_fft)

    copy_diagonal_to_momspace:do ix= 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             do io = 1, nomega
                momentum(ix, iy, iz)%GF(io) = &
                     & workreal(ix, iy, iz, ix, iy, iz)%GF(io)
             enddo
          enddo
       enddo
    enddo copy_diagonal_to_momspace

  contains

    subroutine error_check()

      if (x_size*y_size*z_size.ne.size(rlspace,1)) then
         ierr = 1
      elseif (x_size*y_size*z_size.ne.size(rlspace,2)) then
         ierr = 1
      elseif (size(rlspace,1).ne.size(rlspace,2)) then
         ierr = 2
      else
         ierr = 0
      endif

    end subroutine error_check

  end subroutine realspace_to_momspace

    
  subroutine fft_on_k(workreal, direction)
    type(greensfunc), intent(inout):: workreal(:, :, :, :, :, :)
    integer, intent(in)            :: direction
    
    !routine variables
    type(greensfunc), allocatable :: workslice(:, :, :)
    integer                       :: x_size, y_size, z_size, nomega
    integer                       :: ix, iy, iz
    integer                       :: ierr1

    x_size = size(workreal,1)
    y_size = size(workreal,2)
    z_size = size(workreal,3)
    nomega = size(workreal(1, 1, 1, 1, 1, 1)%GF, 1)
    
    call allocateGF(workslice, x_size, y_size, z_size, nomega, ierr1)
    if (ierr1.ne.0) call fatal_error_from_call(ierr1, "fft_on_k", "allocateGF")

    fft_each_slice_in_turn:do ix= 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             call copyGF (workslice, workreal(1:x_size, 1:y_size,&
                  & 1:z_size, ix, iy, iz))
             
             call gf_fft(workslice, direction, ierr1)
             if (ierr1.ne.0) then
                write (*, *) "Slice labels (x, y, z): ", ix, iy, iz
                call fatal_error_from_call(ierr1, "fft_on_k", "gf_fft")
             endif
             
             call copyGF(workreal(1:x_size, 1:y_size, 1:z_size, ix,&
                  & iy, iz), workslice)
          enddo
       enddo
    enddo fft_each_slice_in_turn

  end subroutine fft_on_k

  subroutine fft_on_kdash(workreal, direction)
    type(greensfunc), intent(inout):: workreal(:, :, :, :, :, :)
    integer, intent(in)            :: direction
    
    !routine variables
    type(greensfunc), allocatable :: workslice(:, :, :)
    integer                       :: x_size, y_size, z_size, nomega
    integer                       :: ix, iy, iz
    integer                       :: ierr1
    
    x_size = size(workreal,4)
    y_size = size(workreal,5)
    z_size = size(workreal,6)
    nomega = size(workreal(1, 1, 1, 1, 1, 1)%GF, 1)

    allocate(workslice(x_size, y_size, z_size)
    call allocate_gf(workslice, nomega)

    fft_each_slice_in_turn:do ix= 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             call copyGF (workslice, workreal(ix, iy, iz, 1:x_size,&
                  & 1:y_size, 1:z_size))
             
             call gf_fft(workslice, direction, ierr1)
             if (ierr1.ne.0) then
                write (*, *) "Slice labels (x, y, z): ", ix, iy, iz
                call fatal_error_from_call(ierr1, "fft_on_kdash", "gf_fft")
             endif
             
             call copyGF(workreal(ix, iy, iz, 1:x_size, 1:y_size,&
                  & 1:z_size), workslice)
          enddo
       enddo
    enddo fft_each_slice_in_turn
    
  end subroutine fft_on_kdash   
   
end submodule gf_transforms
