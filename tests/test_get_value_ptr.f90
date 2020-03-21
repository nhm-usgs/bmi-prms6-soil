    program test_get_value_ptr

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

    contains

! Test getting r32 by nhru.
    function test1() result(code)
    character (len=*), parameter :: &
        var_name = "infil"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
        expected = (/0.02,0.02,0.03,0.04,0.02,0.01,0.03,0.01,0.04,0.09,0.05,0.08,0.05,0.06 /)
    real, pointer :: tptr(:)
    integer :: i, code

    double precision :: endtime
    
    code = m%initialize(config_file)
    code = m%get_end_time(endtime)
    do i = 1,int(endtime)
        status = m%update()
        if(i == endtime) then
            code = m%set_value(var_name, expected)
            code = m%get_value_ptr(var_name, tptr)
        endif
    enddo
    !status = m%get_value(var_name, tval)
    code = m%finalize()


    ! Visual inspection.
    write(*,*) "Test 1"
    write(*,*) "Expected"
    call print_1darray(expected, shape)
    write(*,*) "Get Value Ptr"
    call print_1darray(tptr, shape)
    
    code = BMI_SUCCESS
    do i = 1, shape(1)
        if (tptr(i).ne.expected(i)) then
            code = BMI_FAILURE
            exit
        end if
    end do

    end function test1
! Test getting r64 hru_area.
  function test2() result(status)
    character (len=*), parameter :: &
         var_name = "pfr_dunnian_flow"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    double precision, parameter, dimension(shape(1)) :: &
         expected = (/ 0.00D+000, 0.00D+000, 0.00D+000, 0.00D+000, &
                      0.00D+000, 0.00D+000, 0.00D+000, 0.00D+000, &
                      0.00D+000, 0.00D+000, 0.00D+000, 0.00D+000, &
                      0.00D+000, 0.00D+000 /)
    double precision, pointer :: tptr(:)

    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    do i = 1,int(endtime)
        status = m%update()
        if(i == endtime) then
            status = m%get_value_ptr(var_name, tptr)
        endif
    enddo
    !status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 2"
    write(*,*) "Expected"
    call print_d_1darray(expected, shape)
    write(*,*) "Get Value Ptr"
    call print_d_1darray(tptr, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tptr(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test2

end program test_get_value_ptr
