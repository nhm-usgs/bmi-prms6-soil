program test_update

  use bmif_2_0, only: BMI_FAILURE
  use bmiprmssoil
  use fixtures, only: config_file, status

  implicit none

  double precision, parameter :: expected_time = 1.d0

  type (bmi_prms_soil) :: m
  double precision :: time

  status = m%initialize(config_file)
  status = m%update()
  status = m%get_current_time(time)
  status = m%finalize()

  if (time /= expected_time) then
     stop BMI_FAILURE
  end if
end program test_update
