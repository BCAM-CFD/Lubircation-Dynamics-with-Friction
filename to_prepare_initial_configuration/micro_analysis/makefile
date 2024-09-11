FILE_O : class_files_utilities.o main.o
FILES : class_files_utilities.f90 main.f90
	gfortran FILES -o FILES_O 
%.o : %.f90
	gfortran -c -o $@ $<
%.o : %.mod
class_files_utilities.o : inc_obtain_list_files.f90 inc_search_unit.f90 inc_file_number_lines.f90 inc_file_number_columns.f90 inc_file_to_array.f90
main.o : class_files_utilities.mod
%.mod : %.o
	@
link:
	gfortran *.o -o micro_program
compile:
	make
	make link
clean: 
	rm *.o
	rm *.mod
