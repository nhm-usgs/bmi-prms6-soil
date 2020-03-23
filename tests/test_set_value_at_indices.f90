    program test_set_value_at_indices

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


    end program test_set_value_at_indices
