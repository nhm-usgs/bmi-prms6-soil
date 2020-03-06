program test_get_value_at_indices

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmssoil
    use fixtures, only: status, print_1darray, print_i_1darray, &
        isreal4equalreal4

    implicit none

    character (len=*), parameter :: config_file = "./pipestem/control.simple1"
    type (bmi_prms_soil) :: m
    integer :: retcode


    retcode = test1()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    contains

! Test getting i32 hru_type.
  function test1() result(status)
    character (len=*), parameter :: &
         var_name = "ssr2gw_exp"
    integer, parameter :: rank = 1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(7) :: &
       indices = (/ 2, 4, 6, 8, 10, 12, 14 /)
    real, parameter, dimension(size) :: &
         expected = (/ 1.20, 1.20, 1.20, 1.20, &
                      1.20, 1.20, 1.20 /)
    real :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_value_at_indices(var_name, tval, indices)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    call print_1darray(tval, shape)
    call print_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test1


end program test_get_value_at_indices
