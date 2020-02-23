program test_set_value

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmssoil
    use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
        isReal8EqualReal8, print_i_1darray, print_array, isintEqualint
  implicit none

    type (bmi_prms_soil) :: m
    integer :: retcode

  retcode = test1()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

  !retcode = test2()
  !if (retcode.ne.BMI_SUCCESS) then
  !   stop BMI_FAILURE
  !end if
  !
  !retcode = test3()
  !if (retcode.ne.BMI_SUCCESS) then
  !   stop BMI_FAILURE
  !end if
  !
    contains

      ! Test getting logical val .
  function test1() result(code)
    character (len=*), parameter :: &
         var_name = "pref_flow_den"
    integer, parameter :: rank =1
    integer, parameter :: size = 7
    integer, parameter :: fsize = 14
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(rank) :: fshape = (/ 14 /)
    integer, parameter, dimension(size) :: &
       indices = (/ 2, 4, 6, 8, 10, 12, 14 /)

    real, parameter :: expected(fsize) = (/ 0.0, 0.25, 0.0, 0.25, 0.0, 0.25, 0.0, &
                                              0.25, 0.0, 0.25, 0.0, 0.25, 0.0, 0.25 /)
    real :: val(size), fval(fsize)
    real :: setv(size), fsetv(fsize)
    integer :: i, code

    code = m%initialize(config_file)
    code = m%get_value(var_name, fval)
    code = m%get_value_at_indices(var_name, val, indices)
    val = 0.25
    code = m%set_value_at_indices(var_name, indices, val)
    code = m%get_value_at_indices(var_name, val, indices)
    code = m%get_value(var_name, fval)
    code = m%finalize()
    
    ! Visual inspection.
    write(*,*) "Test 1"
    write(*,*) "Expected"
    call print_1darray(expected, fshape)
    write(*,*) "Set Value"
    call print_1darray(fval, fshape)

    code = BMI_SUCCESS
    do i = 1, fsize
       if (fval(i).ne.expected(i)) then
          code = BMI_FAILURE
       end if
    end do
  end function test1

  !function test1() result(code)
  !  character (len=*), parameter :: &
  !       var_name = "plate_surface__temperature"
  !  integer, parameter :: rank = 2
  !  integer, parameter :: size = 50
  !  integer, parameter :: shape(rank) = (/ 10, 5 /)
  !  real :: x(size), y(size)
  !  integer :: i, code
  !
  !  status = m%initialize(config_file)
  !  status = m%get_value(var_name, x)
  !  x = 42.0
  !  status = m%set_value(var_name, x)
  !  status = m%get_value(var_name, y)
  !  status = m%finalize()
  !
  !  ! Visual inspection.
  !  write(*,*) "Test 1"
  !  call print_array(x, shape)
  !  call print_array(y, shape)
  !
  !  code = BMI_SUCCESS
  !  do i = 1, product(shape)
  !     if (x(i).ne.y(i)) then
  !        code = BMI_FAILURE
  !        exit
  !     end if
  !  end do
  !end function test1
  !
  !function test2() result(code)
  !  character (len=*), parameter :: &
  !       var_name = "plate_surface__thermal_diffusivity"
  !  integer, parameter :: size = 1
  !  real, parameter :: expected(size) = (/ 0.75 /)
  !  real :: x(size), y(size)
  !  integer :: i, code
  !
  !  status = m%initialize(config_file)
  !  status = m%get_value(var_name, x)
  !  status = m%set_value(var_name, expected)
  !  status = m%get_value(var_name, y)
  !  status = m%finalize()
  !
  !  ! Visual inspection.
  !  write(*,*) "Test 2"
  !  write(*,*) x
  !  write(*,*) expected
  !  write(*,*) y
  !
  !  code = BMI_SUCCESS
  !  do i = 1, size
  !     if (y(i).ne.expected(i)) then
  !        code = BMI_FAILURE
  !     end if
  !  end do
  !end function test2
  !
  !function test3() result(code)
  !  character (len=*), parameter :: &
  !       var_name = "model__identification_number"
  !  integer, parameter :: size = 1
  !  integer, parameter :: expected(size) = (/ 42 /)
  !  integer :: x(size), y(size)
  !  integer :: i, code
  !
  !  status = m%initialize(config_file)
  !  status = m%get_value(var_name, x)
  !  status = m%set_value(var_name, expected)
  !  status = m%get_value(var_name, y)
  !  status = m%finalize()
  !
  !  ! Visual inspection.
  !  write(*,*) "Test 3"
  !  write(*,*) x
  !  write(*,*) expected
  !  write(*,*) y
  !
  !  code = BMI_SUCCESS
  !  do i = 1, size
  !     if (y(i).ne.expected(i)) then
  !        code = BMI_FAILURE
  !     end if
  !  end do
  !end function test3

end program test_set_value
