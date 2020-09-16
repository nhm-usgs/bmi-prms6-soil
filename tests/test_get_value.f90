program test_get_value

  use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
  use bmiprmssoil
  use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
      isReal8EqualReal8, print_i_1darray, print_array, isintEqualint, print_d_1darray

  implicit none

  type (bmi_prms_soil) :: m
  integer :: retcode

  !test r32 hru_area
  retcode = test1()
  if (retcode.ne.BMI_SUCCESS) then
    stop BMI_FAILURE
  end if

  ! test r64 by 1
  ! removed because var was removed
!   retcode = test2()
!   if (retcode.ne.BMI_SUCCESS) then
!      stop BMI_FAILURE
!   end if

    !test r64 by hru
  retcode = test3()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

    !test nowtime i(6)
  retcode = test4()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

contains


  ! Test getting r32 ssr2gw_exp.
  function test1() result(status)
    character (len=*), parameter :: &
         var_name = "ssr2gw_exp"
    integer, parameter :: rank = 1
    integer, parameter :: size = 14
    integer, parameter, dimension(rank) :: shape = (/ 14 /)
    real, parameter, dimension(shape(1)) :: &
         expected = (/ 1.20, 1.20, 1.20, 1.20, &
                      1.20, 1.20, 1.20, 1.20, &
                      1.20, 1.20, 1.20, 1.20, &
                      1.20, 1.20 /)
    real :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    do i = 1,int(endtime)
        status = m%update()
        if(i == endtime) then
            status = m%get_value(var_name, tval)
        endif
    enddo
    !status = m%get_value(var_name, tval)
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

  ! Test r64 by 1 .
  function test2() result(status)
    character (len=*), parameter :: &
         var_name = "last_ssstor"
    integer, parameter :: rank = 1
    integer, parameter :: size = 1
    integer, parameter, dimension(rank) :: shape = (/ 1 /)
    double precision, parameter, dimension(shape(1)) :: &
         expected = (/ 3.639392509716796D-004 /)
    double precision :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    do i = 1,int(endtime)
        status = m%update()
        if(i == endtime) then
            status = m%get_value(var_name, tval)
        endif
    enddo
    !status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 2"
    call print_d_1darray(tval, shape)
    call print_d_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test2

  ! Test r64 by nhru
  function test3() result(status)
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
    double precision :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    do i = 1,int(endtime)
        status = m%update()
        if(i == endtime) then
            status = m%get_value(var_name, tval)
        endif
    enddo
    !status = m%get_value(var_name, tval)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 3"
    call print_d_1darray(tval, shape)
    call print_d_1darray(expected, shape)

    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test3

  !test nowtime
  function test4() result(status)
    character (len=*), parameter :: &
         var_name = "nowtime"
    integer, parameter :: rank = 1
    integer, parameter :: size = 6
    integer, parameter, dimension(rank) :: shape = (/ 6 /)
    integer, parameter, dimension(shape(1)) :: &
         expected = (/ 2016, 1, 31, 0, 0, 0 /)
    integer :: tval(size)
    integer :: i, status
    double precision :: endtime
    
    status = m%initialize(config_file)
    status = m%get_end_time(endtime)
    do i = 1,int(endtime)
        status = m%update()
        if(i == endtime) then
            status = m%get_value(var_name, tval)
        endif
    enddo
    !status = m%get_value(var_name, tval)
    status = m%finalize()
  
    ! Visual inspection.
    write(*,*) "Test 4"
    call print_i_1darray(tval, shape)
    call print_i_1darray(expected, shape)
  
    status = BMI_SUCCESS
    do i = 1, size
       if (tval(i).ne.expected(i)) then
          status = BMI_FAILURE
       end if
    end do
  end function test4
  
  end program test_get_value
