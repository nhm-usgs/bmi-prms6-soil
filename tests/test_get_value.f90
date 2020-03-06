program test_get_value

  use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
  use bmiprmssoil
  use fixtures, only: status, print_1darray, isReal4EqualReal4, &
      isReal8EqualReal8, print_i_1darray

  implicit none

  character (len=*), parameter :: config_file = "./pipestem/control.simple1"
  type (bmi_prms_soil) :: m
  integer :: retcode

  !test r32 hru_area
  retcode = test1()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

  !!test r64 by 1
  !retcode = test2()
  !if (retcode.ne.BMI_SUCCESS) then
  !   stop BMI_FAILURE
  !end if


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

  !! Test getting logical val .
  !function test2() result(status)
  !  character (len=*), parameter :: &
  !       var_name = "last_soil_moist"
  !  integer, parameter :: size = 1
  !  double precision, parameter :: expected(size) = (/ 0.76300 /)
  !  double precision :: val(size)
  !  integer :: i, status
  !  double precision :: endtime
  !  
  !  status = m%initialize(config_file)
  !  status = m%get_end_time(endtime)
  !  do i = 1,int(endtime)
  !      status = m%update()
  !  enddo
  !  status = m%get_value(var_name, val)
  !  status = m%finalize()
  !
  !  ! Visual inspection.
  !  write(*,*) "Test 1"
  !  write(*,*) "Expected", expected
  !  write(*,*) "Get Value", val
  !  
  !  status = BMI_SUCCESS
  !  do i = 1, size
  !     if (val(i).ne.expected(i)) then
  !        status = BMI_FAILURE
  !     end if
  !  end do  
  !end function test2

end program test_get_value
