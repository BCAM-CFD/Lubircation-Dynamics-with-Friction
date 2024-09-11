FILE_O : class_read_input.o class_files_utilities.o class_file.o class_cell.o class_particle.o class_cell_system.o class_computational.o class_coord.o class_length.o class_montecarlo.o class_neighbours.o class_potential.o class_physics.o class_random.o class_sim.o main.o
FILES : class_read_input.f90 class_files_utilities.f90 class_file.f90 class_cell.f90 class_particle.f90 class_cell_system.f90 class_computational.f90 class_coord.f90 class_length.f90 class_montecarlo.f90 class_neighbours.f90 class_potential.f90 class_physics.f90 class_random.f90 class_sim.f90 main.f90
	gfortran FILES -o FILES_O 
%.o : %.f90
	gfortran -c -o $@ $<
%.o : %.mod
class_cell.o : class_computational.mod class_length.mod class_coord.mod inc_remove_part_from_cell.f90 inc_check_part_cell.f90 inc_cell_destructor.f90
class_cell_system.o : class_cell.mod inc_cell_system_constructor.f90 inc_calculate_cells_list.f90 inc_change_cell_i.f90 inc_calculate_cell_i.f90 inc_add_part_to_cell.f90 inc_cell_system_destructor.f90 inc_write_cell_system_info.f90
class_coord.o : inc_coord_destructor.f90
class_file.o : class_computational.mod inc_file_constructor.f90 inc_update_name.f90 inc_file_destructor.f90 inc_write_file_info.f90
class_files_utilities.o : inc_obtain_list_files.f90 inc_obtain_list_dirs.f90 inc_search_unit.f90 inc_file_number_lines.f90 inc_file_number_columns.f90 inc_file_to_array.f90 inc_check_directory.f90
class_length.o : class_computational.mod
class_montecarlo.o : class_computational.mod inc_read_montecarlo.f90 inc_write_MC_info.f90 inc_write_montecarlo.f90 inc_initialize_montecarlo.f90 inc_check_accept.f90 inc_change_amp.f90 inc_check_montecarlo.f90
class_particle.o : class_computational.mod class_neighbours.mod inc_create_particle.f90 inc_destroy_particle.f90
class_physics.o : class_computational.mod class_particle.mod inc_read_physics.f90 inc_initialize_physics.f90 inc_initialize_positions.f90 inc_write_pos.f90 inc_write_energy.f90 inc_write_physics_info.f90 inc_read_pos.f90 inc_compute_energy.f90 inc_compute_energy_i.f90 inc_check_compute_energy.f90 inc_change_pos.f90 inc_destroy_physics.f90 inc_calculate_Rij_sq.f90 inc_check_physics.f90
class_potential.o : class_computational.mod inc_read_potential.f90 inc_initialize_potential.f90 inc_build_table.f90 inc_potential.f90 inc_potential_table.f90 inc_write_potential.f90 inc_write_potential_table.f90 inc_potential_destructor.f90 inc_write_potential_info.f90 inc_check_potential.f90
class_random.o : class_computational.mod inc_read_random.f90 inc_init_random_seed.f90 inc_random_constructor.f90 inc_random_gaussian.f90 inc_write_random_info.f90
class_read_input.o : inc_read_input_constructor.f90 inc_find_string.f90 inc_obtain_variable_string.f90 inc_get_variable.f90 inc_get_format.f90 inc_take_size_array.f90 inc_read_variable.f90 inc_search_unit_b.f90 inc_number_checkings.f90
class_sim.o : class_computational.mod class_montecarlo.mod class_physics.mod class_random.mod class_file.mod class_cell_system.mod inc_read_input.f90 inc_initialize_sim.f90 inc_initialize_files.f90 inc_read_sim.f90 inc_write_info.f90 inc_write_all_files_info.f90 inc_destroy_sim.f90 inc_check_sim.f90
main.o : class_sim.mod
%.mod : %.o
	@
link:
	gfortran *.o -o montecarlo
compile:
	make
	make link
clean: 
	rm *.o
	rm *.mod
