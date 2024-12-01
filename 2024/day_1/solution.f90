module solution_module
   implicit none
contains
   function solve_part1(input_path) result(total_distance)
      character(len=*), intent(in) :: input_path
      integer :: total_distance
      integer, allocatable :: left_list(:), right_list(:)
      integer :: i, io_status, num_lines

      ! First, count the number of lines in the file
      num_lines = 0
      open(unit=10, file=input_path, status='old', action='read', iostat=io_status)
      if (io_status /= 0) then
         print *, "Error opening input file"
         stop
      end if

      do
         read(10, *, iostat=io_status)
         if (io_status /= 0) exit
         num_lines = num_lines + 1
      end do
      rewind(10)

      ! Allocate arrays based on number of lines
      allocate(left_list(num_lines))
      allocate(right_list(num_lines))

      ! Read the two columns into the arrays
      do i = 1, num_lines
         read(10, *, iostat=io_status) left_list(i), right_list(i)
         if (io_status /= 0) then
            print *, "Error reading line", i
            stop
         end if
      end do

      close(10)

      ! Sort both lists
      call sort_array(left_list, num_lines)
      call sort_array(right_list, num_lines)

      ! Calculate the total distance
      total_distance = 0
      do i = 1, num_lines
         total_distance = total_distance + abs(left_list(i) - right_list(i))
      end do

      ! Deallocate arrays
      deallocate(left_list)
      deallocate(right_list)
   end function solve_part1

   ! Subroutine to sort an array using Bubble Sort
   subroutine sort_array(array, n)
      integer, intent(inout) :: array(:)
      integer, intent(in) :: n
      integer :: i, j, temp
      do i = 1, n-1
         do j = 1, n-i
            if (array(j) > array(j+1)) then
               temp = array(j)
               array(j) = array(j+1)
               array(j+1) = temp
            end if
         end do
      end do
   end subroutine sort_array

   function solve_part2(input_path) result(similarity_score)
      character(len=*), intent(in) :: input_path
      integer :: similarity_score
      integer, allocatable :: left_list(:), right_list(:)
      integer :: i, j, io_status, num_lines, appearances

      ! First, count the number of lines in the file
      num_lines = 0
      open(unit=10, file=input_path, status='old', action='read', iostat=io_status)
      if (io_status /= 0) then
         print *, "Error opening input file"
         stop
      end if

      do
         read(10, *, iostat=io_status)
         if (io_status /= 0) exit
         num_lines = num_lines + 1
      end do
      rewind(10)

      ! Allocate arrays based on number of lines
      allocate(left_list(num_lines))
      allocate(right_list(num_lines))

      ! Read the two columns into the arrays
      do i = 1, num_lines
         read(10, *, iostat=io_status) left_list(i), right_list(i)
         if (io_status /= 0) then
            print *, "Error reading line", i
            stop
         end if
      end do

      close(10)

      ! Calculate similarity score
      similarity_score = 0
      do i = 1, num_lines
         appearances = 0
         ! Count appearances of left_list(i) in right_list
         do j = 1, num_lines
            if (left_list(i) == right_list(j)) then
               appearances = appearances + 1
            end if
         end do
         similarity_score = similarity_score + (left_list(i) * appearances)
      end do

      ! Deallocate arrays
      deallocate(left_list)
      deallocate(right_list)
   end function solve_part2
end module solution_module

program reconcile_lists
   use solution_module
   implicit none
   integer :: length, total_distance, similarity_score
   character(len=256) :: program_path, program_dir, input_file

   ! Get the program path
   call get_command_argument(0, program_path, length)
   program_dir = program_path(1:scan(program_path, '/', back=.true.))
   input_file = trim(program_dir) // 'input.txt'

   ! Solve part 1
   total_distance = solve_part1(input_file)
   print *, "Part 1 - The total distance between the lists is:", total_distance

   ! Solve part 2
   similarity_score = solve_part2(input_file)
   print *, "Part 2 - The similarity score is:", similarity_score

end program reconcile_lists
