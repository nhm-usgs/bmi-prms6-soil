    module bmiprmssoil

    use m_prms_soil
    use bmif_2_0
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
    implicit none

    type, extends (bmi) :: bmi_prms_soil
        private
        type (prms_soil_model) :: model
    contains
    procedure :: get_component_name => prms_component_name
    procedure :: get_input_item_count => prms_input_item_count
    procedure :: get_output_item_count => prms_output_item_count
    procedure :: get_input_var_names => prms_input_var_names
    procedure :: get_output_var_names => prms_output_var_names
    procedure :: initialize => prms_initialize
    procedure :: finalize => prms_finalize
    procedure :: get_start_time => prms_start_time
    procedure :: get_end_time => prms_end_time
    procedure :: get_current_time => prms_current_time
    procedure :: get_time_step => prms_time_step
    procedure :: get_time_units => prms_time_units
    procedure :: update => prms_update
    procedure :: update_until => prms_update_until
    procedure :: get_var_grid => prms_var_grid
    procedure :: get_grid_type => prms_grid_type
    procedure :: get_grid_rank => prms_grid_rank
    procedure :: get_grid_shape => prms_grid_shape
    procedure :: get_grid_size => prms_grid_size
    procedure :: get_grid_spacing => prms_grid_spacing
    procedure :: get_grid_origin => prms_grid_origin
    procedure :: get_grid_x => prms_grid_x
    procedure :: get_grid_y => prms_grid_y
    procedure :: get_grid_z => prms_grid_z
    procedure :: get_grid_node_count => prms_grid_node_count
    procedure :: get_grid_edge_count => prms_grid_edge_count
    procedure :: get_grid_face_count => prms_grid_face_count
    procedure :: get_grid_edge_nodes => prms_grid_edge_nodes
    procedure :: get_grid_face_edges => prms_grid_face_edges
    procedure :: get_grid_face_nodes => prms_grid_face_nodes
    procedure :: get_grid_nodes_per_face => prms_grid_nodes_per_face
    procedure :: get_var_type => prms_var_type
    procedure :: get_var_units => prms_var_units
    procedure :: get_var_itemsize => prms_var_itemsize
    procedure :: get_var_nbytes => prms_var_nbytes
    procedure :: get_var_location => prms_var_location
    procedure :: get_value_int => prms_get_int
    procedure :: get_value_float => prms_get_float
    procedure :: get_value_double => prms_get_double
    generic :: get_value => &
         get_value_int, &
         get_value_float, &
         get_value_double
    procedure :: get_value_ptr_int => prms_get_ptr_int
    procedure :: get_value_ptr_float => prms_get_ptr_float
    procedure :: get_value_ptr_double => prms_get_ptr_double
    generic :: get_value_ptr => &
         get_value_ptr_int, &
         get_value_ptr_float, &
         get_value_ptr_double
    procedure :: get_value_at_indices_int => prms_get_at_indices_int
    procedure :: get_value_at_indices_float => prms_get_at_indices_float
    procedure :: get_value_at_indices_double => prms_get_at_indices_double
    generic :: get_value_at_indices => &
         get_value_at_indices_int, &
         get_value_at_indices_float, &
         get_value_at_indices_double
    procedure :: set_value_int => prms_set_int
    procedure :: set_value_float => prms_set_float
    procedure :: set_value_double => prms_set_double
    generic :: set_value => &
         set_value_int, &
         set_value_float, &
         set_value_double
    procedure :: set_value_at_indices_int => prms_set_at_indices_int
    procedure :: set_value_at_indices_float => prms_set_at_indices_float
    procedure :: set_value_at_indices_double => prms_set_at_indices_double
    generic :: set_value_at_indices => &
         set_value_at_indices_int, &
         set_value_at_indices_float, &
         set_value_at_indices_double
    !procedure :: print_model_info
    end type bmi_prms_soil

    private
    public :: bmi_prms_soil

    character (len=BMI_MAX_COMPONENT_NAME), target :: &
        component_name = "prms6-BMI-SOIL"

    ! Exchange items
    integer, parameter :: input_item_count = 32
    integer, parameter :: output_item_count = 49
    character (len=BMI_MAX_VAR_NAME), target, &
        dimension(input_item_count) :: &
        input_items = (/&
        !values below are required by soilzone module ffrom the surface zone
        !runoff
        'dprst_evap_hru      ', & !r32 by nhru
        'dprst_seep_hru      ', & !r64 by nhru
        'hru_area_perv       ', & !r32 by nhru
        'hru_frac_perv       ', & !r32 by nhru
        'hru_impervevap      ', & !r32 by nhru
        'infil               ', & !r32 by nhru
        'soil_moist_chg      ', & !r32 by nhru
        'soil_rechr_chg      ', & !r32 by nhru
        'sroff               ', & !r32 by nhru
        'srunoff_updated_soil', & !logical by 1
        'strm_seg_in         ', & !r64 by nsegment not yet implemented
        
        !potet
        'potet               ', & !r32 by nhru

        !precipitation
        'hru_ppt             ', & !r32 by nhru
        
        !transpiration
        'transp_on           ', & !logical by nhru

        !intcp
        'hru_intcpevap       ', & !r32 by nhru

        !snow
        'snow_evap           ', & !r32 by nhru
        'snowcov_area        ', & !r32 by nhru

        !climate
        'soil_rechr          ', & !r32 by nhru
        'soil_rechr_max      ', & !r32 by nhru and calibration
        'soil_moist          ', & !r32 by nhru
        'soil_moist_max      ', & !r32 by nhru and calibration
        !###################################################!
        ! first cut at                                      !
        !other values that could be used during calibration !
        !these are soilzone vars                            !
    
        'pref_flow_den       ', & !r32 by nhru
        'pref_flow_max       ', & !r32 by nhru
        'pref_flow_thrsh     ', & !r32 by nhru
        'soil2gw_max         ', & !r32 by nhru
        'ssr2gw_exp          ', & !r32 by nhru
        'ssr2gw_rate         ', & !r32 by nhru
        'sat_threshold       ', & !r32 by nhru
        'slowcoef_lin        ', & !r32 by nhru
        'slowcoef_sq         ', & !r32 by nhru
        'fastcoef_lin        ', & !r32 by nhru
        'fastcoef_sq         ' & !r32 by nhru
        /)
        
    character (len=BMI_MAX_VAR_NAME), target, &
        dimension(output_item_count) :: &
        output_items = (/ &
        !soil module
        'soil_moist_tot      ', & !r32 by nhru transfer to gw
        'soil_to_gw          ', & !r32 by nhru transfer to gw
        'ssr_to_gw           ', & !r32 by nhru transfer to gw
        'ssres_flow          ', & !r32 by nhru transfer to gw and streamflow
        !soilzone vars used in water-balance
        'cap_infil_tot       ', & !r32 by nhru
        'cap_waterin         ', & !r32 by nhru
        'dunnian_flow        ', & !r32 by nhru
        'grav_dunnian_flow   ', & !r32 by nhru
        'gvr2pfr             ', & !r32 by nhru
        'hru_sz_cascadeflow  ', & !r32 by nhru
        'perv_actet          ', & !r32 by nhru
        'pref_flow_den       ', & !r32 by nhru
        'pref_flow_infil     ', & !r32 by nhru
        'pref_flow_max       ', & !r32 by nhru
        'pref_flow_stor      ', & !r32 by nhru
        'pref_flow_thrsh     ', & !r32 by nhru
        'soil_lower          ', & !r32 by nhru
        'soil_to_ssr         ', & !r32 by nhru
        'ssres_in            ', & !r32 by nhru
        'swale_actet         ', & !r32 by nhru
        ! 2 below not yet active will ! for now
        'upslope_dunnianflow ', &!r64 by nhru
        'upslope_interflow   ', & !r64 by nhru
        'pfr_dunnian_flow    ', & !r64 by nhru
        'last_soil_moist     ',& !r64 by 1
        'last_ssstor         ', & !r64 by 1
        !soilzone water-balance and pot calibration target
        'hru_actet           ', & !r32 by nhru
        'ssres_stor          ', & !r32 by nhru
        'pref_flow           ', & !r32 by nhru
        'slow_flow           ', & !r32 by nhru
        'slow_stor           ', & !r32 by nhru
        
        !climate
        'soil_moist          ', & !r32 by nhru transfer to surface
        'soil_rechr          ', & !r32 by nhru transfer to surface
        
        !runoff
        'infil               ', & !r32 by nhru transfer to surface
        'sroff               ', & !r32 by nhru transfer to surface
        'strm_seg_in         ', & !r64 by nhru transfer to surface not yet implemented
        
        !prsm_time
        'nowtime             ', & !i32(6)

    !###################################################!
        ! first cut at                                      !
        !other values that could be used during calibration !
        !these are soilzone vars                            !
        !climate
        'soil_rechr_max      ', & !r32 by nhru 
        'soil_moist_max      ', & !r32 by nhru 
        !soil
        'pref_flow_den       ', & !r32 by nhru
        'pref_flow_max       ', & !r32 by nhru
        'pref_flow_thrsh     ', & !r32 by nhru
        'soil2gw_max         ', & !r32 by nhru
        'ssr2gw_exp          ', & !r32 by nhru
        'ssr2gw_rate         ', & !r32 by nhru
        'sat_threshold       ', & !r32 by nhru
        'slowcoef_lin        ', & !r32 by nhru
        'slowcoef_sq         ', & !r32 by nhru
        'fastcoef_lin        ', & !r32 by nhru
        'fastcoef_sq         ' & !r32 by nhru
        /)

    contains

    ! Get the name of the model.
    function prms_component_name(this, name) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
    end function prms_component_name

    ! Count the input variables.
    function prms_input_item_count(this, count) result (bmi_status)
        class (bmi_prms_soil), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status

        count = input_item_count
        bmi_status = BMI_SUCCESS
     end function prms_input_item_count

    ! Count the output variables.
    function prms_output_item_count(this, count) result (bmi_status)
        class (bmi_prms_soil), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status

        count = output_item_count
        bmi_status = BMI_SUCCESS
    end function prms_output_item_count

    ! List input variables.
    function prms_input_var_names(this, names) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    names => input_items
    bmi_status = BMI_SUCCESS
    end function prms_input_var_names

    ! List output variables.
    function prms_output_var_names(this, names) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    names => output_items
    bmi_status = BMI_SUCCESS
    end function prms_output_var_names

    ! BMI initializer.
    function prms_initialize(this, config_file) result (bmi_status)
    class (bmi_prms_soil), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
        call initialize_from_file(this%model, config_file)
    else
        !call initialize_from_defaults(this%model)
    end if
    bmi_status = BMI_SUCCESS
    end function prms_initialize

    ! BMI finalizer.
    function prms_finalize(this) result (bmi_status)
    class (bmi_prms_soil), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
    end function prms_finalize

    ! Model start time.
    function prms_start_time(this, time) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    !time = this%model%model_simulation%model_time%Timestep
    bmi_status = BMI_SUCCESS
    end function prms_start_time

    ! Model end time.
    function prms_end_time(this, time) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%model_simulation%model_time%Number_timesteps)
    bmi_status = BMI_SUCCESS
    end function prms_end_time

    ! Model current time.
    function prms_current_time(this, time) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%model_simulation%model_time%Timestep)
    bmi_status = BMI_SUCCESS
    end function prms_current_time

    ! Model time step.
    function prms_time_step(this, time_step) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%model_simulation%model_time%Timestep_seconds)
    bmi_status = BMI_SUCCESS
    end function prms_time_step

    ! Model time units.
    function prms_time_units(this, units) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
    end function prms_time_units

    ! Advance model by one time step.
    function prms_update(this) result (bmi_status)
    class (bmi_prms_soil), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
    end function prms_update

    ! Advance the model until the given time.
    function prms_update_until(this, time) result (bmi_status)
    class (bmi_prms_soil), intent(inout) :: this
    double precision, intent(in) :: time
    double precision :: current_time, end_time, dt
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s
    s = this%get_current_time(current_time)
    s = this%get_end_time(end_time)
    s = this%get_time_step(dt)
    if (time > current_time) then
        n_steps_real = (time - current_time)
        n_steps = floor(n_steps_real)
        do i = 1, n_steps
            s = this%update()
        end do
        !s = this%update_frac(n_steps_real - dble(n_steps))
    end if
    bmi_status = BMI_SUCCESS
    end function prms_update_until

    ! Get the grid id for a particular variable.
    function prms_var_grid(this, name, grid) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case( &
            !runoff
        'dprst_evap_hru', 'dprst_seep_hru','hru_area_perv', 'hru_frac_perv', &
        'infil', 'sroff', 'hru_impervevap', 'soil_moist_chg', 'soil_rechr_chg', &
        
            !precipitation
        'hru_ppt', & 
            !potet
        'potet', &
        
            !traspiration
        'transp_on', &
        
            !intcp
        'hru_intcpevap', &
            
            !snow
        'snow_evap', 'snowcov_area', &
        
            !climate
        'soil_rechr', 'soil_rechr_max', 'soil_moist', 'soil_moist_max', &
        
            !soil (this)
        'soil_moist_tot', 'soil_to_gw', 'ssr_to_gw', 'ssres_flow', &
        'pref_flow_den', 'pref_flow_max', 'pref_flow_thrsh', 'soil2gw_max', & 
        'ssr2gw_exp',  'ssr2gw_rate', 'sat_threshold',  'slowcoef_lin', & 
        'slowcoef_sq',  'fastcoef_lin', 'fastcoef_sq', 'cap_infil_tot', & 
        'cap_waterin', 'dunnian_flow', 'grav_dunnian_flow', 'gvr2pfr', & 
        'hru_sz_cascadeflow', &
        'perv_actet', &
        'pref_flow_infil', 'pref_flow_stor', & 
        'soil_lower', & 
        'soil_to_ssr', 'ssres_in', 'swale_actet', &
        'upslope_dunnianflow',  'upslope_interflow', &
        'pfr_dunnian_flow', & 
        'hru_actet',  'ssres_stor', 'pref_flow', 'slow_flow',  'slow_stor')
        grid = 0
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        grid = 1
        bmi_status = BMI_SUCCESS
    case('srunoff_updated_soil', 'last_soil_moist', & 
        'last_ssstor')
        grid = 2
        bmi_status = BMI_SUCCESS
    case('nowtime')
        grid = 3    
        bmi_status = BMI_SUCCESS
    case default
        grid = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_grid

    ! The type of a variable's grid.
    function prms_grid_type(this, grid, type) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case(1)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case(2)
        type = 'scalar'
        bmi_status = BMI_SUCCESS
    case(3)
        type = 'vector'
        bmi_status = BMI_SUCCESS
    case default
        type = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_type

    ! The number of dimensions of a grid.
    function prms_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(1)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(2)
        rank = 0
        bmi_status = BMI_SUCCESS
    case(3)
        rank = 1
        bmi_status = BMI_SUCCESS
    case default
        rank = -1
        bmi_status = BMI_FAILURE
    end select    
    end function prms_grid_rank

    ! The dimensions of a grid.
    function prms_grid_shape(this, grid, shape) result (bmi_status)
      class (bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: shape
      integer :: bmi_status
    
      select case(grid)
      case(0)
         shape(:) = [this%model%model_simulation%model_basin%nhru]
         bmi_status = BMI_SUCCESS
     case(1)
         shape(:) = [this%model%model_simulation%model_basin%nsegment]
         bmi_status = BMI_SUCCESS
     case(2)
         shape(:) = [1]
         bmi_status = BMI_SUCCESS
     case(3)
         shape(:) = [6]
         bmi_status = BMI_SUCCESS
     case default
         shape(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_shape
    
    ! The total number of elements in a grid.
    function prms_grid_size(this, grid, size) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
        size = this%model%model_simulation%model_basin%nhru
        bmi_status = BMI_SUCCESS
    case(1)
        size = this%model%model_simulation%model_basin%nsegment
        bmi_status = BMI_SUCCESS
    case(2)
        size = 1
        bmi_status = BMI_SUCCESS
    case(3)
        size = 6
        bmi_status = BMI_SUCCESS
    case default
        size = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_size

    ! The distance between nodes of a grid.
    function prms_grid_spacing(this, grid, spacing) result (bmi_status)
     class (bmi_prms_soil), intent(in) :: this
     integer, intent(in) :: grid
     double precision, dimension(:), intent(out) :: spacing
     integer :: bmi_status

     select case(grid)
     case default
        spacing(:) = -1.d0
        bmi_status = BMI_FAILURE
     end select
    end function prms_grid_spacing
    
    ! Coordinates of grid origin.
    function prms_grid_origin(this, grid, origin) result (bmi_status)
      class (bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      double precision, dimension(:), intent(out) :: origin
      integer :: bmi_status
    
      select case(grid)
      case default
         origin(:) = -1.d0
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_origin
    
    ! X-coordinates of grid nodes.
    function prms_grid_x(this, grid, x) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(0)
        x = this%model%model_simulation%model_basin%hru_x
        bmi_status = BMI_SUCCESS
    case(1) 
        bmi_status = this%get_value('nhm_seg', x)
    case(2)
        x = -1.d0
        bmi_status = BMI_SUCCESS
    case(3)
        x = dble([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
        bmi_status = BMI_SUCCESS
    case default
        x = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_x

    ! Y-coordinates of grid nodes.
    function prms_grid_y(this, grid, y) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(0)
        y = this%model%model_simulation%model_basin%hru_y
        bmi_status = BMI_SUCCESS
    case(1) 
        y(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case(2)
        y = -1.d0
        bmi_status = BMI_SUCCESS
    case(3)
        y(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case default
        y = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_y

    ! Z-coordinates of grid nodes.
    function prms_grid_z(this, grid, z) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(0)
        z = this%model%model_simulation%model_basin%hru_elev
        bmi_status = BMI_SUCCESS
    case(1) 
        z(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case(2)
        z = -1.d0
        bmi_status = BMI_SUCCESS
    case(3)
        z(:) = -1.d0
        bmi_status = BMI_SUCCESS
    case default
        z = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_z

    ! Get the number of nodes in an unstructured grid.
    function prms_grid_node_count(this, grid, count) result(bmi_status)
      class(bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case(0:3)
         bmi_status = this%get_grid_size(grid, count)
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_node_count

    ! Get the number of edges in an unstructured grid.
    function prms_grid_edge_count(this, grid, count) result(bmi_status)
      class(bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case(0:3)
         bmi_status = this%get_grid_node_count(grid, count)
         count = count - 1
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_edge_count

    ! Get the number of faces in an unstructured grid.
    function prms_grid_face_count(this, grid, count) result(bmi_status)
      class(bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case(0:3)
         count = 0
         bmi_status = BMI_SUCCESS
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_count

    ! Get the edge-node connectivity.
    function prms_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
      class(bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: edge_nodes
      integer :: bmi_status

      select case(grid)
      case default
         edge_nodes(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_edge_nodes

    ! Get the face-edge connectivity.
    function prms_grid_face_edges(this, grid, face_edges) result(bmi_status)
      class(bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_edges
      integer :: bmi_status

      select case(grid)
      case default
         face_edges(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_edges

    ! Get the face-node connectivity.
    function prms_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
      class(bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_nodes
      integer :: bmi_status

      select case(grid)
      case default
         face_nodes(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_nodes

    ! Get the number of nodes for each face.
    function prms_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
      class(bmi_prms_soil), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: nodes_per_face
      integer :: bmi_status

      select case(grid)
      case default
         nodes_per_face(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_nodes_per_face

    ! The data type of the variable, as a string.
    function prms_var_type(this, name, type) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case( &
            !precipitation
        "hru_ppt", &
        
            !runoff
        'dprst_evap_hru', 'infil', 'sroff', 'hru_area_perv', 'hru_frac_perv', &
        'hru_impervevap', 'soil_moist_chg', 'soil_rechr_chg', &
        
            !potet
        'potet', &
        
            !intcp
        'hru_intcpevap', &
        
            !snow
        'snow_evap', 'snowcov_area', &
        
            !climate
        'soil_rechr_max', 'soil_moist', 'soil_moist_max', &
        
        
            !soil (this)
        'soil_moist_tot', 'soil_to_gw', 'ssr_to_gw', 'ssres_flow', &
        'pref_flow_den', 'pref_flow_max', 'pref_flow_thrsh', 'soil2gw_max', & 
        'ssr2gw_exp',  'ssr2gw_rate', 'sat_threshold',  'slowcoef_lin', & 
        'slowcoef_sq',  'fastcoef_lin', 'fastcoef_sq','cap_infil_tot', & 
        'cap_waterin', 'dunnian_flow', & 
        'hru_sz_cascadeflow', &
        'perv_actet', &
        'pref_flow_infil', 'pref_flow_stor', & 
        'soil_lower',  & 
        'soil_to_ssr', 'ssres_in', 'swale_actet', &
        'pfr_dunnian_flow', & 
        'hru_actet',  'ssres_stor', 'pref_flow', 'slow_flow',  'slow_stor', &
        'soil_rechr')
        type = "real"
        bmi_status = BMI_SUCCESS
    case('strm_seg_in', &
        'dprst_seep_hru', 'grav_dunnian_flow', 'gvr2pfr', &
        'prf_dunnian_flow', 'upslope_dunnianflow', 'upslope_interflow', &
        'last_soil_moist', 'last_ssstor')
        type = "double precision"
        bmi_status = BMI_SUCCESS
    case('nowtime')
        type = "integer"
        bmi_status = BMI_SUCCESS
    case('srunoff_updated_soil', 'transp_on')
        type = 'integer'
        bmi_status = BMI_SUCCESS
    case default
        type = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_type

    ! The units of the given variable.
    function prms_var_units(this, name, units) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status
    
    bmi_status = BMI_SUCCESS

    select case(name)
    case( &
        !precipitation
        "hru_ppt", &
        
        !runoff
        'dprst_evap_hru', 'infil', 'sroff', 'soil_moist_chg', 'soil_rechr_chg', &
        'dprst_seep_hru', 'hru_impervevap', &
        !potet
        'potet', &
        
        !intcp
        'hru_intcpevap', &
        
        !snow
        'snow_evap', &
        
        !climate
        'soil_rechr', 'soil_rechr_max', 'soil_moist', 'soil_moist_max', &
        
        !potet
        
        !soil (this)
        'soil_moist_tot', 'soil_to_gw', 'ssr_to_gw', 'ssres_flow', &
        'pref_flow_max', 'pref_flow_thrsh', 'hru_sz_cascadeflow', &
        'upslope_dunnianflow', 'upslope_interflow', 'soil2gw_max', &
        'sat_threshold', 'cap_infil_tot', 'cap_waterin', 'dunnian_flow', &
        'grav_dunnian_flow', 'gvr2pfr', 'perv_actet', 'pref_flow_infil', &
        'pref_flow_stor', 'soil_lower', 'soil_to_ssr', 'ssres_in', &
        'swale_actet','pfr_dunnian_flow', 'last_soil_moist', 'last_ssstor', &
        'hru_actet', 'ssres_stor', 'pref_flow', 'slow_stor', 'slow_flow')
        units = "in"
    case('strm_seg_in')
        units = "ft3 s-1"
    case('snowcov_area', 'hru_area_perv')
        units = 'acres'       
    case('hru_frac_perv', 'pref_flow_den')
        units = 'fraction'
    case('ssr2gw_rate', 'slowcoef_lin', 'fastcoef_lin')
        units = 'fraction day-1'
    case('srunoff_updated_soil', 'transp_on', 'ssr2gw_exp', 'slowcoef_sq', &
        'fastcoef_sq', 'nowtime')
        units = 'none'

    case default
        units = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_units

    ! Memory use per array element.
    function prms_var_itemsize(this, name, size) result (bmi_status)
      class (bmi_prms_soil), intent(in) :: this
      character (len=*), intent(in) :: name
      integer, intent(out) :: size
      integer :: bmi_status
    
    select case(name)
    case('nowtime')
        size = sizeof(this%model%model_simulation%model_time%nowtime(1))
        bmi_status = BMI_SUCCESS
    case('srunoff_updated_soil')
        size = sizeof(this%model%model_simulation%runoff%srunoff_updated_soil)
        bmi_status = BMI_SUCCESS
    case('transp_on')
        size = sizeof(this%model%model_simulation%transpiration%transp_on(1))
        bmi_status = BMI_SUCCESS
    case('hru_ppt')
        size = sizeof(this%model%model_simulation%model_precip%hru_ppt(1))
        bmi_status = BMI_SUCCESS
    case('dprst_evap_hru')
        size = sizeof(this%model%model_simulation%runoff%dprst_evap_hru(1))
        bmi_status = BMI_SUCCESS
    case('infil')
        size = sizeof(this%model%model_simulation%runoff%infil(1))
        bmi_status = BMI_SUCCESS
    case('sroff')
        size = sizeof(this%model%model_simulation%runoff%sroff(1))
        bmi_status = BMI_SUCCESS
    case('potet')
        size = sizeof(this%model%model_simulation%potet%potet(1))
        bmi_status = BMI_SUCCESS
    case('hru_intcpevap')
        size = sizeof(this%model%model_simulation%intcp%hru_intcpevap(1))
        bmi_status = BMI_SUCCESS
    case('snow_evap')
        size = sizeof(this%model%model_simulation%snow%snow_evap(1))
        bmi_status = BMI_SUCCESS
    case('snowcov_area')
        size = sizeof(this%model%model_simulation%snow%snowcov_area(1))
        bmi_status = BMI_SUCCESS
    case('soil_rechr')
        size = sizeof(this%model%model_simulation%climate%soil_rechr(1))
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max')
        size = sizeof(this%model%model_simulation%climate%soil_rechr_max(1))
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        size = sizeof(this%model%model_simulation%climate%soil_moist(1))
        bmi_status = BMI_SUCCESS
    case('soil_moist_max')
        size = sizeof(this%model%model_simulation%climate%soil_moist_max(1))
        bmi_status = BMI_SUCCESS
    case('hru_area_perv')
        size = sizeof(this%model%model_simulation%runoff%hru_area_perv(1))
        bmi_status = BMI_SUCCESS
    case('hru_impervevap')
        size = sizeof(this%model%model_simulation%runoff%hru_impervevap(1))
        bmi_status = BMI_SUCCESS
    case('soil_moist_chg')
        size = sizeof(this%model%model_simulation%runoff%soil_moist_chg(1))
        bmi_status = BMI_SUCCESS
    case('soil_rechr_chg')
        size = sizeof(this%model%model_simulation%runoff%soil_rechr_chg(1))
        bmi_status = BMI_SUCCESS
    case('dprst_seep_hru')
        size = sizeof(this%model%model_simulation%runoff%dprst_seep_hru(1))
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            size = sizeof(this%model%model_simulation%runoff%strm_seg_in(1))
            bmi_status = BMI_SUCCESS
        else
            size = -1
            bmi_status = BMI_SUCCESS
        endif
    case('hru_frac_perv')
        size = sizeof(this%model%model_simulation%runoff%hru_frac_perv(1))
        bmi_status = BMI_SUCCESS

        !soilzone vars
    case('soil_moist_tot')
        size = sizeof(this%model%model_simulation%soil%soil_moist_tot(1))
        bmi_status = BMI_SUCCESS
    case('soil_to_gw')
        size = sizeof(this%model%model_simulation%soil%soil_to_gw(1))
        bmi_status = BMI_SUCCESS
    case('ssr_to_gw')
        size = sizeof(this%model%model_simulation%soil%ssr_to_gw(1))
        bmi_status = BMI_SUCCESS
    case('ssres_flow')
        size = sizeof(this%model%model_simulation%soil%ssres_flow(1))
        bmi_status = BMI_SUCCESS
    case('pref_flow_den')
        size = sizeof(this%model%model_simulation%soil%pref_flow_den(1))
        bmi_status = BMI_SUCCESS
    case('pref_flow_max')
        size = sizeof(this%model%model_simulation%soil%pref_flow_max(1))
        bmi_status = BMI_SUCCESS
    case('pref_flow_thrsh')
        size = sizeof(this%model%model_simulation%soil%pref_flow_thrsh(1))
        bmi_status = BMI_SUCCESS
    case('soil2gw_max')
        size = sizeof(this%model%model_simulation%soil%soil2gw_max(1))
        bmi_status = BMI_SUCCESS
    case('ssr2gw_exp')
        size = sizeof(this%model%model_simulation%soil%ssr2gw_exp(1))
        bmi_status = BMI_SUCCESS
    case('ssr2gw_rate')
        size = sizeof(this%model%model_simulation%soil%ssr2gw_rate(1))
        bmi_status = BMI_SUCCESS
    case('sat_threshold')
        size = sizeof(this%model%model_simulation%soil%sat_threshold(1))
        bmi_status = BMI_SUCCESS
    case('slowcoef_lin')
        size = sizeof(this%model%model_simulation%soil%slowcoef_lin(1))
        bmi_status = BMI_SUCCESS
    case('slowcoef_sq')
        size = sizeof(this%model%model_simulation%soil%slowcoef_sq(1))
        bmi_status = BMI_SUCCESS
    case('fastcoef_lin')
        size = sizeof(this%model%model_simulation%soil%fastcoef_lin(1))
        bmi_status = BMI_SUCCESS
    case('fastcoef_sq')
        size = sizeof(this%model%model_simulation%soil%fastcoef_sq(1))
        bmi_status = BMI_SUCCESS
        
    case('cap_infil_tot')
       size = sizeof(this%model%model_simulation%soil%cap_infil_tot(1))
        bmi_status = BMI_SUCCESS
    case('cap_waterin')
        size = sizeof(this%model%model_simulation%soil%cap_waterin(1))
        bmi_status = BMI_SUCCESS
    case('dunnian_flow') 
       size = sizeof(this%model%model_simulation%soil%dunnian_flow(1))
        bmi_status = BMI_SUCCESS
    case('hru_sz_cascadeflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            size = sizeof(this%model%model_simulation%soil%hru_sz_cascadeflow(1))
            bmi_status = BMI_SUCCESS
        else
            size = -1
            bmi_status = BMI_SUCCESS
        endif
    case('perv_actet')
        size = sizeof(this%model%model_simulation%soil%perv_actet(1))
        bmi_status = BMI_SUCCESS
    case('pref_flow_infil')
        size = sizeof(this%model%model_simulation%soil%pref_flow_infil(1))
        bmi_status = BMI_SUCCESS
    case('pref_flow_stor')
        size = sizeof(this%model%model_simulation%soil%pref_flow_stor(1))
        bmi_status = BMI_SUCCESS
    case('soil_lower')
        size = sizeof(this%model%model_simulation%soil%soil_lower(1))
        bmi_status = BMI_SUCCESS
    case('soil_to_ssr')
        size = sizeof(this%model%model_simulation%soil%soil_to_ssr(1))
        bmi_status = BMI_SUCCESS
    case('ssres_in')
        size = sizeof(this%model%model_simulation%soil%ssres_in(1))
        bmi_status = BMI_SUCCESS
    case('swale_actet')
        size = sizeof(this%model%model_simulation%soil%swale_actet(1))
        bmi_status = BMI_SUCCESS
    case('hru_actet')
        size = sizeof(this%model%model_simulation%soil%hru_actet(1))
        bmi_status = BMI_SUCCESS
    case('ssres_stor')
        size = sizeof(this%model%model_simulation%soil%ssres_stor(1))
        bmi_status = BMI_SUCCESS
    case('pref_flow')
        size = sizeof(this%model%model_simulation%soil%pref_flow(1))
        bmi_status = BMI_SUCCESS
    case('slow_flow')
        size = sizeof(this%model%model_simulation%soil%slow_flow(1))
        bmi_status = BMI_SUCCESS
    case('slow_stor')
        size = sizeof(this%model%model_simulation%soil%slow_stor(1))
        bmi_status = BMI_SUCCESS

    case('grav_dunnian_flow')
        size = sizeof(this%model%model_simulation%soil%grav_dunnian_flow(1))
        bmi_status = BMI_SUCCESS
    case('gvr2pfr')
        size = sizeof(this%model%model_simulation%soil%gvr2pfr(1))
        bmi_status = BMI_SUCCESS
    case('pfr_dunnian_flow')
        size = sizeof(this%model%model_simulation%soil%pfr_dunnian_flow(1))
        bmi_status = BMI_SUCCESS
    case('upslope_dunnianflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            size = sizeof(this%model%model_simulation%soil%upslope_dunnianflow(1))
            bmi_status = BMI_SUCCESS
        else
            size = -1
            bmi_status = BMI_SUCCESS
        endif
    case('upslope_interflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            size = sizeof(this%model%model_simulation%soil%upslope_interflow(1))
            bmi_status = BMI_SUCCESS
        else
            size = -1
            bmi_status = BMI_SUCCESS
        endif
    case('last_soil_moist')
        size = sizeof(this%model%model_simulation%soil%last_soil_moist)
        bmi_status = BMI_SUCCESS
    case('last_ssstor')
        size = sizeof(this%model%model_simulation%soil%last_ssstor)
        bmi_status = BMI_SUCCESS
    case default
        size = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_itemsize
    
    ! The size of the given variable.
    function prms_var_nbytes(this, name, nbytes) result (bmi_status)
      class (bmi_prms_soil), intent(in) :: this
      character (len=*), intent(in) :: name
      integer, intent(out) :: nbytes
      integer :: bmi_status
      integer :: s1, s2, s3, type, grid_size, item_size
    
      s1 = this%get_var_grid(name, type)
      s2 = this%get_grid_size(type, grid_size)
      s3 = this%get_var_itemsize(name, item_size)
    
      if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
         nbytes = item_size * grid_size
         bmi_status = BMI_SUCCESS
      else
         nbytes = -1
         bmi_status = BMI_FAILURE
      end if
    end function prms_var_nbytes
    
  ! The location (node, face, edge) of the given variable.
    function prms_var_location(this, name, location) result (bmi_status)
        class (bmi_prms_soil), intent(in) :: this
        character (len=*), intent(in) :: name
        character (len=*), intent(out) :: location
        integer :: bmi_status

        select case(name)
        case default
           location = "node"
           bmi_status = BMI_SUCCESS
        end select
    end function prms_var_location
    
    ! Get a copy of a integer variable's values, flattened.
    function prms_get_int(this, name, dest) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case('nowtime')
        dest = [this%model%model_simulation%model_time%nowtime]
        bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE 
    end select
    end function prms_get_int
    
    ! Get a copy of a real variable's values, flattened.
    function prms_get_float(this, name, dest) result (bmi_status)
      class (bmi_prms_soil), intent(in) :: this
      character (len=*), intent(in) :: name
      real, intent(inout) :: dest(:)
      integer :: bmi_status
    
    select case(name)
    case('infil')
        dest = [this%model%model_simulation%runoff%infil]
        bmi_status = BMI_SUCCESS
    case('sroff')
        dest = [this%model%model_simulation%runoff%sroff]
        bmi_status = BMI_SUCCESS
    case('soil_rechr')
        dest = [this%model%model_simulation%climate%soil_rechr]
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        dest = [this%model%model_simulation%climate%soil_moist]
        bmi_status = BMI_SUCCESS
    case('soil_moist_tot')
        dest = [this%model%model_simulation%soil%soil_moist_tot]
        bmi_status = BMI_SUCCESS
    case('soil_to_gw')
        dest = [this%model%model_simulation%soil%soil_to_gw]
        bmi_status = BMI_SUCCESS
    case('ssr_to_gw')
        dest = [this%model%model_simulation%soil%ssr_to_gw]
        bmi_status = BMI_SUCCESS
    case('ssres_flow')
        dest = [this%model%model_simulation%soil%ssres_flow]
        bmi_status = BMI_SUCCESS

    case('cap_infil_tot')
        dest = [this%model%model_simulation%soil%cap_infil_tot]
        bmi_status = BMI_SUCCESS
    case('cap_waterin')
        dest = [this%model%model_simulation%soil%cap_waterin]
        bmi_status = BMI_SUCCESS
    case('dunnian_flow') 
        dest = [this%model%model_simulation%soil%dunnian_flow]
        bmi_status = BMI_SUCCESS
    case('hru_sz_cascadeflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            dest = [this%model%model_simulation%soil%hru_sz_cascadeflow]
            bmi_status = BMI_SUCCESS
        else
            dest(:) = -1.0
            bmi_status = BMI_SUCCESS
        endif
    case('perv_actet')
        dest = [this%model%model_simulation%soil%perv_actet]
        bmi_status = BMI_SUCCESS
    case('pref_flow_infil')
        dest = [this%model%model_simulation%soil%pref_flow_infil]
        bmi_status = BMI_SUCCESS
    case('pref_flow_stor')
        dest = [this%model%model_simulation%soil%pref_flow_stor]
        bmi_status = BMI_SUCCESS
    case('soil_lower')
        dest = [this%model%model_simulation%soil%soil_lower]
        bmi_status = BMI_SUCCESS
    case('soil_to_ssr')
        dest = [this%model%model_simulation%soil%soil_to_ssr]
        bmi_status = BMI_SUCCESS
    case('ssres_in')
        dest = [this%model%model_simulation%soil%ssres_in]
        bmi_status = BMI_SUCCESS
    case('swale_actet')
        dest = [this%model%model_simulation%soil%swale_actet]
        bmi_status = BMI_SUCCESS
    case('hru_actet')
        dest = [this%model%model_simulation%soil%hru_actet]
        bmi_status = BMI_SUCCESS
    case('ssres_stor')
        dest = [this%model%model_simulation%soil%ssres_stor]
        bmi_status = BMI_SUCCESS
    case('pref_flow')
        dest = [this%model%model_simulation%soil%pref_flow]
        bmi_status = BMI_SUCCESS
    case('slow_flow')
        dest = [this%model%model_simulation%soil%slow_flow]
        bmi_status = BMI_SUCCESS
    case('slow_stor')
        dest = [this%model%model_simulation%soil%slow_stor]
        bmi_status = BMI_SUCCESS
        
        !Get Calibration Parmas
    case('soil_rechr_max')
        dest = [this%model%model_simulation%climate%soil_rechr_max]
        bmi_status = BMI_SUCCESS
    case('soil_moist_max')
        dest = [this%model%model_simulation%climate%soil_rechr_max]
        bmi_status = BMI_SUCCESS
        !soil
    case('pref_flow_den')
        dest = [this%model%model_simulation%soil%pref_flow_den]
        bmi_status = BMI_SUCCESS
    case('pref_flow_max')
        dest = [this%model%model_simulation%soil%pref_flow_max]
        bmi_status = BMI_SUCCESS
    case('pref_flow_thrsh')
        dest = [this%model%model_simulation%soil%pref_flow_thrsh]
        bmi_status = BMI_SUCCESS
    case('soil2gw_max')
        dest = [this%model%model_simulation%soil%soil2gw_max]
        bmi_status = BMI_SUCCESS
    case('ssr2gw_exp')
        dest = [this%model%model_simulation%soil%ssr2gw_exp]
        bmi_status = BMI_SUCCESS
    case('ssr2gw_rate')
        dest = [this%model%model_simulation%soil%ssr2gw_rate]
        bmi_status = BMI_SUCCESS
    case('sat_threshold')
        dest = [this%model%model_simulation%soil%sat_threshold]
        bmi_status = BMI_SUCCESS
    case('slowcoef_lin')
        dest = [this%model%model_simulation%soil%slowcoef_lin]
        bmi_status = BMI_SUCCESS
    case('slowcoef_sq')
        dest = [this%model%model_simulation%soil%slowcoef_sq]
        bmi_status = BMI_SUCCESS
    case('fastcoef_lin')
        dest = [this%model%model_simulation%soil%fastcoef_lin]
        bmi_status = BMI_SUCCESS
    case('fastcoef_sq')
        dest = [this%model%model_simulation%soil%fastcoef_sq]
        bmi_status = BMI_SUCCESS

    case default
        dest = [-1.0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_float
    
    ! Get a copy of a double variable's values, flattened.
    function prms_get_double(this, name, dest) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            dest = [this%model%model_simulation%runoff%strm_seg_in]
            bmi_status = BMI_SUCCESS
        else
            dest(:) = -1.d0
            bmi_status = BMI_SUCCESS
        endif
    case('grav_dunnian_flow')
        dest = [this%model%model_simulation%soil%grav_dunnian_flow]
        bmi_status = BMI_SUCCESS
    case('gvr2pfr')
        dest = [this%model%model_simulation%soil%gvr2pfr]
        bmi_status = BMI_SUCCESS
    case('pfr_dunnian_flow')
        dest = [this%model%model_simulation%soil%pfr_dunnian_flow]
        bmi_status = BMI_SUCCESS
    case('upslope_dunnianflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            dest = [this%model%model_simulation%soil%upslope_dunnianflow]
            bmi_status = BMI_SUCCESS
        else
            dest(:) = -1.d0
            bmi_status = BMI_SUCCESS
        endif
    case('upslope_interflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            dest = [this%model%model_simulation%soil%upslope_interflow]
            bmi_status = BMI_SUCCESS
        else
            dest(:) = -1.d0
            bmi_status = BMI_SUCCESS
        endif
    case('last_soil_moist')
        dest = [this%model%model_simulation%soil%last_soil_moist]
        bmi_status = BMI_SUCCESS
    case('last_ssstor')
        dest = [this%model%model_simulation%soil%last_ssstor]
        bmi_status = BMI_SUCCESS
        case default
        dest = [-1.d0]
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_double
    
    ! Get a reference to an integer-valued variable, flattened.
    function prms_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status, status
    type (c_ptr) :: src
    integer :: n_elements, gridid

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select

    end function prms_get_ptr_int

    ! Get a reference to a real-valued variable, flattened.
    function prms_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, gridid, status

    select case(name)
    case('infil')
        src = c_loc(this%model%model_simulation%runoff%infil(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('sroff')
        src = c_loc(this%model%model_simulation%runoff%sroff(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_rechr')
        src = c_loc(this%model%model_simulation%climate%soil_rechr(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        src = c_loc(this%model%model_simulation%climate%soil_moist(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_area_perv')
        src = c_loc(this%model%model_simulation%runoff%hru_area_perv(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_moist_tot')
        src = c_loc(this%model%model_simulation%soil%soil_moist_tot(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_to_gw')
        src = c_loc(this%model%model_simulation%soil%soil_to_gw(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('ssr_to_gw')
        src = c_loc(this%model%model_simulation%soil%ssr_to_gw(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('ssres_flow')
        src = c_loc(this%model%model_simulation%soil%ssres_flow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    
    
    case('cap_infil_tot')
        src = c_loc(this%model%model_simulation%soil%cap_infil_tot(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('cap_waterin')
        src = c_loc(this%model%model_simulation%soil%cap_waterin(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('dunnian_flow') 
        src = c_loc(this%model%model_simulation%soil%dunnian_flow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_sz_cascadeflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%soil%hru_sz_cascadeflow(1))
            status = this%get_var_grid(name,gridid)
            status = this%get_grid_size(gridid, n_elements)
            call c_f_pointer(src, dest_ptr, [n_elements])
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('perv_actet')
        src = c_loc(this%model%model_simulation%soil%perv_actet(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('pref_flow_den')
        src = c_loc(this%model%model_simulation%soil%pref_flow_den(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('pref_flow_infil')
        src = c_loc(this%model%model_simulation%soil%pref_flow_infil(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('pref_flow_max')
        src = c_loc(this%model%model_simulation%soil%pref_flow_max(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('pref_flow_stor')
        src = c_loc(this%model%model_simulation%soil%pref_flow_stor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('pref_flow_thrsh')
        src = c_loc(this%model%model_simulation%soil%pref_flow_thrsh(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_lower')
        src = c_loc(this%model%model_simulation%soil%soil_lower(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('soil_to_ssr')
        src = c_loc(this%model%model_simulation%soil%soil_to_ssr(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('ssres_in')
        src = c_loc(this%model%model_simulation%soil%ssres_in(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('swale_actet')
        src = c_loc(this%model%model_simulation%soil%swale_actet(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_actet')
        src = c_loc(this%model%model_simulation%soil%hru_actet(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('ssres_stor')
        src = c_loc(this%model%model_simulation%soil%ssres_stor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('pref_flow')
        src = c_loc(this%model%model_simulation%soil%pref_flow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('slow_flow')
        src = c_loc(this%model%model_simulation%soil%slow_flow(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('slow_stor')
        src = c_loc(this%model%model_simulation%soil%slow_stor(1))
        status = this%get_var_grid(name,gridid)
        status = this%get_grid_size(gridid, n_elements)
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_ptr_float

    ! Get a reference to an double-valued variable, flattened.
    function prms_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, status, gridid
    
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%runoff%strm_seg_in(1))
            call c_f_pointer(src, dest_ptr, [n_elements])
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('grav_dunnian_flow')
        src = c_loc(this%model%model_simulation%soil%grav_dunnian_flow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('gvr2pfr')
        src = c_loc(this%model%model_simulation%soil%gvr2pfr(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('pfr_dunnian_flow')
        src = c_loc(this%model%model_simulation%soil%pfr_dunnian_flow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('upslope_dunnianflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%soil%upslope_dunnianflow(1))
            call c_f_pointer(src, dest_ptr, [n_elements])
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('upslope_interflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%soil%upslope_interflow(1))
            call c_f_pointer(src, dest_ptr, [n_elements])
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_ptr_double
    !
    function prms_get_at_indices_int(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    integer, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid

    select case(name)
        case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_int

    ! Get values of a real variable at the given locations.
    function prms_get_at_indices_float(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    real, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid
        
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)

    case('infil')
        src = c_loc(this%model%model_simulation%runoff%infil(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('sroff')
        src = c_loc(this%model%model_simulation%runoff%sroff(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_rechr')
        src = c_loc(this%model%model_simulation%climate%soil_rechr(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        src = c_loc(this%model%model_simulation%climate%soil_moist(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1, n_elements
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_moist_tot')
        src = c_loc(this%model%model_simulation%soil%soil_moist_tot(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_to_gw')
        src = c_loc(this%model%model_simulation%soil%soil_to_gw(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('ssr_to_gw')
        src = c_loc(this%model%model_simulation%soil%ssr_to_gw(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('ssres_flow')
        src = c_loc(this%model%model_simulation%soil%ssres_flow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        
    case('cap_infil_tot')
        src = c_loc(this%model%model_simulation%soil%cap_infil_tot(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('cap_waterin')
        src = c_loc(this%model%model_simulation%soil%cap_waterin(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('dunnian_flow') 
        src = c_loc(this%model%model_simulation%soil%dunnian_flow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_sz_cascadeflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%soil%hru_sz_cascadeflow(1))
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('perv_actet')
        src = c_loc(this%model%model_simulation%soil%perv_actet(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('pref_flow_infil')
        src = c_loc(this%model%model_simulation%soil%pref_flow_infil(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('pref_flow_stor')
        src = c_loc(this%model%model_simulation%soil%pref_flow_stor(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_lower')
        src = c_loc(this%model%model_simulation%soil%soil_lower(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_to_ssr')
        src = c_loc(this%model%model_simulation%soil%soil_to_ssr(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('ssres_in')
        src = c_loc(this%model%model_simulation%soil%ssres_in(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('swale_actet')
        src = c_loc(this%model%model_simulation%soil%swale_actet(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_actet')
        src = c_loc(this%model%model_simulation%soil%hru_actet(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('ssres_stor')
        src = c_loc(this%model%model_simulation%soil%ssres_stor(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('pref_flow')
        src = c_loc(this%model%model_simulation%soil%pref_flow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('slow_flow')
        src = c_loc(this%model%model_simulation%soil%slow_flow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('slow_stor')
        src = c_loc(this%model%model_simulation%soil%slow_stor(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        
        !Get Calibration Parmas
    case('soil_rechr_max')
        src = c_loc(this%model%model_simulation%climate%soil_rechr_max(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil_moist_max')
        src = c_loc(this%model%model_simulation%climate%soil_moist_max(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        !soil
    case('pref_flow_den')
        src = c_loc(this%model%model_simulation%soil%pref_flow_den(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('pref_flow_max')
        src = c_loc(this%model%model_simulation%soil%pref_flow_max(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('pref_flow_thrsh')
        src = c_loc(this%model%model_simulation%soil%pref_flow_thrsh(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('soil2gw_max')
        src = c_loc(this%model%model_simulation%soil%soil2gw_max(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('ssr2gw_exp')
        src = c_loc(this%model%model_simulation%soil%ssr2gw_exp(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('ssr2gw_rate')
        src = c_loc(this%model%model_simulation%soil%ssr2gw_rate(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('sat_threshold')
        src = c_loc(this%model%model_simulation%soil%sat_threshold(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('slowcoef_lin')
        src = c_loc(this%model%model_simulation%soil%slowcoef_lin(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('slowcoef_sq')
        src = c_loc(this%model%model_simulation%soil%slowcoef_sq(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('fastcoef_lin')
        src = c_loc(this%model%model_simulation%soil%fastcoef_lin(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('fastcoef_sq')
        src = c_loc(this%model%model_simulation%soil%fastcoef_sq(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS

    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_float

    ! Get values of a double variable at the given locations.
    function prms_get_at_indices_double(this, name, dest, inds) &
        result (bmi_status)
    class (bmi_prms_soil), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    double precision, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid
        
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%runoff%strm_seg_in(1))
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('grav_dunnian_flow')
        src = c_loc(this%model%model_simulation%soil%grav_dunnian_flow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('gvr2pfr')
        src = c_loc(this%model%model_simulation%soil%gvr2pfr(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('pfr_dunnian_flow')
        src = c_loc(this%model%model_simulation%soil%pfr_dunnian_flow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('upslope_dunnianflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%soil%upslope_dunnianflow(1))
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('upslope_interflow')
        if(this%model%control_data%cascade_flag%value == 1) then
            src = c_loc(this%model%model_simulation%soil%upslope_interflow(1))
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_double
    !
    ! Set new integer values.
    function prms_set_int(this, name, src) result (bmi_status)
      class (bmi_prms_soil), intent(inout) :: this
      character (len=*), intent(in) :: name
      integer, intent(in) :: src(:)
      integer :: bmi_status, n_elements, gridid, i, status
      logical, allocatable, dimension(:) :: boolvals
             
      status = this%get_var_grid(name,gridid)
      status = this%get_grid_size(gridid, n_elements)

      select case(name)
      case('srunoff_updated_soil')
          if(src(1) == 0) then
              this%model%model_simulation%runoff%srunoff_updated_soil = .false.
          else
              this%model%model_simulation%runoff%srunoff_updated_soil = .true.
          endif
          bmi_status = BMI_SUCCESS
      case('transp_on')
        allocate(boolvals(n_elements))
        do i = 1,n_elements
          if(src(i).eq.0) then 
              boolvals(i) = .false.
          else 
              boolvals(i) = .true.
          endif
        enddo
          this%model%model_simulation%transpiration%transp_on = boolvals
          bmi_status = BMI_SUCCESS
      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_set_int
    
    ! Set new real values.
    function prms_set_float(this, name, src) result (bmi_status)
      class (bmi_prms_soil), intent(inout) :: this
      character (len=*), intent(in) :: name
      real, intent(in) :: src(:)
      integer :: bmi_status
    
    select case(name)
    case('hru_ppt')
        this%model%model_simulation%model_precip%hru_ppt = src
        bmi_status = BMI_SUCCESS
    case('dprst_evap_hru')
        this%model%model_simulation%runoff%dprst_evap_hru = src
        bmi_status = BMI_SUCCESS
    case('infil')
        this%model%model_simulation%runoff%infil = src
        bmi_status = BMI_SUCCESS
    case('sroff')
        this%model%model_simulation%runoff%sroff = src
        bmi_status = BMI_SUCCESS
    case('potet')
        this%model%model_simulation%potet%potet = src
        bmi_status = BMI_SUCCESS
    case('hru_intcpevap')
        this%model%model_simulation%intcp%hru_intcpevap = src
        bmi_status = BMI_SUCCESS
    case('snow_evap')
        this%model%model_simulation%snow%snow_evap = src
        bmi_status = BMI_SUCCESS
    case('snowcov_area')
        this%model%model_simulation%snow%snowcov_area = src
        bmi_status = BMI_SUCCESS
    case('soil_rechr')
        this%model%model_simulation%climate%soil_rechr = src
        bmi_status = BMI_SUCCESS
    case('soil_rechr_max')
        this%model%model_simulation%climate%soil_rechr_max = src
        bmi_status = BMI_SUCCESS
    case('soil_moist')
        this%model%model_simulation%climate%soil_moist = src
        bmi_status = BMI_SUCCESS
    case('soil_moist_max')
        this%model%model_simulation%climate%soil_moist_max = src
        bmi_status = BMI_SUCCESS
    case('hru_area_perv')
        this%model%model_simulation%runoff%hru_area_perv = src
        bmi_status = BMI_SUCCESS
    case('hru_impervevap')
        this%model%model_simulation%runoff%hru_impervevap = src
        bmi_status = BMI_SUCCESS
    case('soil_moist_chg')
        this%model%model_simulation%runoff%soil_moist_chg = src
        bmi_status = BMI_SUCCESS
    case('soil_rechr_chg')
        this%model%model_simulation%runoff%soil_rechr_chg = src
        bmi_status = BMI_SUCCESS
    case('hru_frac_perv')
        this%model%model_simulation%runoff%hru_frac_perv = src
        bmi_status = BMI_SUCCESS
    case('pref_flow_den')
        this%model%model_simulation%soil%pref_flow_den = src
        bmi_status = BMI_SUCCESS
    case('pref_flow_max')
        this%model%model_simulation%soil%pref_flow_max = src
        bmi_status = BMI_SUCCESS
    case('pref_flow_thrsh')
        this%model%model_simulation%soil%pref_flow_thrsh = src
        bmi_status = BMI_SUCCESS
    case('soil2gw_max')
        this%model%model_simulation%soil%soil2gw_max = src
        bmi_status = BMI_SUCCESS
    case('ssr2gw_exp')
        this%model%model_simulation%soil%ssr2gw_exp = src
        bmi_status = BMI_SUCCESS
    case('ssr2gw_rate')
        this%model%model_simulation%soil%ssr2gw_rate = src
        bmi_status = BMI_SUCCESS
    case('sat_threshold')
        this%model%model_simulation%soil%sat_threshold = src
        bmi_status = BMI_SUCCESS
    case('slowcoef_lin')
        this%model%model_simulation%soil%slowcoef_lin = src
        bmi_status = BMI_SUCCESS
    case('slowcoef_sq')
        this%model%model_simulation%soil%slowcoef_sq = src
        bmi_status = BMI_SUCCESS
    case('fastcoef_lin')
        this%model%model_simulation%soil%fastcoef_lin = src
        bmi_status = BMI_SUCCESS
    case('fastcoef_sq')
        this%model%model_simulation%soil%fastcoef_sq = src
        bmi_status = BMI_SUCCESS

    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_float
    
    ! Set new double values.
    function prms_set_double(this, name, src) result (bmi_status)
      class (bmi_prms_soil), intent(inout) :: this
      character (len=*), intent(in) :: name
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    
    select case(name)
    case('dprst_seep_hru')
        this%model%model_simulation%runoff%dprst_seep_hru = src
        bmi_status = BMI_SUCCESS
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            this%model%model_simulation%runoff%strm_seg_in = src
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_set_double
    
    ! Set integer values at particular locations.
    function prms_set_at_indices_int(this, name, inds, src) &
        result (bmi_status)
    class (bmi_prms_soil), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    integer, pointer :: dest_flattened(:)
    integer :: i
    
      select case(name)
      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_set_at_indices_int
    
    ! Set real values at particular locations.
    function prms_set_at_indices_float(this, name, inds, src) &
        result (bmi_status)
    class (bmi_prms_soil), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    real, pointer :: dest_flattened(:)
    integer :: i, n_elements, status, gridid
   
    status = this%get_var_grid(name, gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    case('pref_flow_den')
        dest = c_loc(this%model%model_simulation%soil%pref_flow_den(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('pref_flow_max')
        dest = c_loc(this%model%model_simulation%soil%pref_flow_max(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('pref_flow_thrsh')
       dest = c_loc( this%model%model_simulation%soil%pref_flow_thrsh(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('soil2gw_max')
       dest = c_loc(this%model%model_simulation%soil%soil2gw_max(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('ssr2gw_exp')
        dest = c_loc(this%model%model_simulation%soil%ssr2gw_exp(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('ssr2gw_rate')
        dest = c_loc(this%model%model_simulation%soil%ssr2gw_rate(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('sat_threshold')
        dest = c_loc(this%model%model_simulation%soil%sat_threshold(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('slowcoef_lin')
        dest = c_loc(this%model%model_simulation%soil%slowcoef_lin(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('slowcoef_sq')
        dest = c_loc(this%model%model_simulation%soil%slowcoef_sq(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('fastcoef_lin')
        dest = c_loc(this%model%model_simulation%soil%fastcoef_lin(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
    case('fastcoef_sq')
        dest = c_loc(this%model%model_simulation%soil%fastcoef_sq(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS

      case default

         bmi_status = BMI_FAILURE
      end select
    end function prms_set_at_indices_float
    
    ! Set double values at particular locations.
    function prms_set_at_indices_double(this, name, inds, src) &
        result (bmi_status)
    class (bmi_prms_soil), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    double precision, pointer :: dest_flattened(:)
    integer :: i, n_elements, status, gridid
    
    select case(name)
    case('strm_seg_in')
        if(this%model%control_data%cascade_flag%value == 1) then
            dest = c_loc(this%model%model_simulation%runoff%strm_seg_in(1))
            call c_f_pointer(dest, dest_flattened, [n_elements])
            do i = 1, size(inds)
                dest_flattened(inds(i)) = src(i)
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case default
         bmi_status = BMI_FAILURE
    end select
    end function prms_set_at_indices_double
    !
    !! A non-BMI procedure for model introspection.
    !subroutine print_model_info(this)
    !  class (bmi_prms_soil), intent(in) :: this
    !
    !  call print_info(this%model)
    !end subroutine print_model_info

    end module bmiprmssoil
