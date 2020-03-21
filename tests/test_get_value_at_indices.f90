program test_get_value_at_indices

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmssoil
    use fixtures, only: config_file, status, print_1darray, print_i_1darray, &
        isreal4equalreal4, print_d_1darray

    implicit none

    type (bmi_prms_soil) :: m
    integer :: retcode


    retcode = test1()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    retcode = test2()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if
    
    !retcode = test6()
    !if (retcode.ne.BMI_SUCCESS) then
    !    stop BMI_FAILURE
    !end if
    
    contains

! Test getting r32 hru_type.
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
  
! Test getting r64 by nhru
    function test2() result(code)
    character (len=*), parameter :: &
         var_name = "pfr_dunnian_flow"
    integer, parameter :: rank =1
    integer, parameter :: size = 7
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(size) :: &
       indices = (/ 2, 4, 6, 8, 10, 12, 14 /)

    double precision, parameter :: expected(size) = (/ 0.00D+000, 0.00D+000, 0.00D+000, 0.00D+000, &
                      0.00D+000, 0.00D+000, 0.00D+000 /)
    double precision :: val(size)
    integer :: i, code
    double precision :: endtime

    code = m%initialize(config_file)
    code = m%get_end_time(endtime)
    do i = 1,int(endtime)
        code = m%update()
        if(i == endtime) then
            code = m%get_value_at_indices(var_name, val, indices)
        endif
    enddo
    
    ! Visual inspection.
    write(*,*) "Test 2"
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)
    write(*,*) "get Value"
    call print_d_1darray(val, shape)

    code = BMI_SUCCESS
    do i = 1, size
       if (val(i).ne.expected(i)) then
          code = BMI_FAILURE
       end if
    end do
  end function test2

end program test_get_value_at_indices
