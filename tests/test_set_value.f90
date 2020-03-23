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

    contains

  ! Test getting r32 hru_area.
  function test1() result(code)
    character (len=*), parameter :: &
         var_name = "pref_flow_den"
    integer, parameter :: size = 14
    integer :: dims(1) = size
    real, parameter :: expected(size) = (/ 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, &
                                              0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25 /)
    real :: val(size), val2(size)
    integer :: i, code

    code = m%initialize(config_file)
    code = m%get_value(var_name, val)
    val = 0.25
    code = m%set_value(var_name, val)
    code = m%get_value(var_name, val2)
    code = m%finalize()
    
    ! Visual inspection.
    write(*,*) "Test 1"
    call print_1darray(val, dims)
    call print_1darray(val2, dims)

    code = BMI_SUCCESS
    do i = 1, size
       if (val2(i).ne.expected(i)) then
          code = BMI_FAILURE
       end if
    end do
  end function test1

  
end program test_set_value
