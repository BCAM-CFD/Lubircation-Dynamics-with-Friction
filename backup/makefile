FILE_O : class_cell.o class_comp_time.o class_computational.o class_files_utilities.o  class_functions_utilities.o class_file.o class_read_input.o class_input.o class_output.o class_particle.o class_random.o class_system.o class_wall.o main.o
FILES : class_cell.f90 class_comp_time.f90 class_computational.f90 class_files_utilities.f90  class_functions_utilities.f90 class_file.f90 class_read_input.f90 class_input.f90 class_output.f90 class_particle.f90 class_random.f90 class_system.f90 class_wall.f90 main.f90
	gfortran FILES -o FILES_O 
%.o : %.f90
	ifort -c -O3 -o $@ $<
#	gfortran -c -O2 -o $@ $<
%.o : %.mod
class_cell.o : class_computational.mod inc_cell_constructor.f90 inc_cell_info.f90 inc_cell_destructor.f90
class_comp_time.o : class_computational.mod inc_initialize_comp_time.f90 inc_write_comp_times.f90
class_computational.o : inc_error_header.f90
class_file.o : class_computational.mod inc_file_constructor.f90 inc_update_name.f90 inc_file_destructor.f90 inc_write_file_info.f90
class_files_utilities.o : inc_obtain_list_files.f90 inc_obtain_list_dirs.f90 inc_search_unit.f90 inc_file_number_lines.f90 inc_file_number_columns.f90 inc_file_to_array.f90 inc_check_directory.f90
class_functions_utilities.o : inc_mean_value.f90 inc_bit_function.f90 inc_data_delimiter.f90 inc_lin_regression.f90
class_input.o : class_computational.mod inc_read_input.f90 inc_input_destructor.f90
class_output.o : class_computational.mod class_file.mod inc_create_output.f90 inc_write_output_info.f90 inc_destroy_output.f90
class_particle.o : class_computational.mod inc_particle_constructor.f90 inc_particle_info.f90 inc_particle_destructor.f90
class_random.o : class_computational.mod inc_read_random.f90 inc_init_random_seed.f90 inc_random_constructor.f90 inc_random_gaussian.f90 inc_write_random_info.f90
class_read_input.o : class_computational.mod inc_read_input_constructor.f90 inc_find_string.f90 inc_obtain_variable_string.f90 inc_get_variable.f90 inc_get_format.f90 inc_take_size_array.f90 inc_read_variable.f90 inc_search_unit_b.f90 inc_number_checkings.f90
class_system.o : class_computational.mod class_particle.mod class_random.mod class_output.mod class_cell.mod class_wall.mod inc_create_system.f90 inc_initial_positions.f90 inc_initial_radius.f90 inc_initial_mass.f90 inc_initial_velocities.f90 inc_initial_angular_velocities.f90 inc_write_info.f90 inc_write_particles.f90 inc_initial_cells.f90 inc_cells_edges.f90 inc_cells_neighbours.f90 inc_calculate_cells_list.f90 inc_calculate_neigh_list.f90 inc_calculate_neigh.f90 inc_initial_neigh_list.f90 inc_check_update_neigh.f90 inc_forces.f90 inc_friction_coeff.f90 inc_forces_explicit.f90 inc_VV.f90 inc_VV_explicit.f90 inc_PBC.f90 inc_vel_implicit_lub.f90 inc_vel_implicit_lub_biviscous.f90 inc_vel_iterative.f90 inc_vel_iterative_biviscous.f90 inc_write_particles_info.f90 inc_write_cells_info.f90 inc_destroy_system.f90 inc_check_pos_walls.f90 inc_check_part_dist.f90 inc_write_neighbours.f90 inc_write_wall.f90 inc_write_Nsweeps.f90 inc_write_stress.f90 inc_bulk.f90 inc_stress.f90 inc_calculate_Rij_sq.f90 inc_compute_shear_rate.f90 inc_write_shear_rate.f90
class_wall.o : class_computational.mod inc_wall_constructor.f90 inc_wall_destructor.f90 inc_initial_wall.f90 inc_write_wall_info.f90
main.o : class_system.mod class_input.mod class_comp_time.mod
%.mod : %.o
	@
link:
	ifort *.o -o SpinCoderealmu
compile:
	make
	make link
clean: 
	rm *.o
	rm *.mod
