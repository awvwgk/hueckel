* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1711
*     Copyright (C) 2017 awvwgk
*
*     This program is free software: you can redistribute it and/or 
*     modify it under the terms of the GNU General Public License as 
*     published by the Free Software Foundation, either version 3 of 
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program.  If not, see www.gnu.org/licenses/.
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1711
      subroutine rdhmat(fname,nao,nel,hmat)
      implicit none
      character(len=*),intent(in) :: fname
      integer,intent(out) :: nao,nel
      real*8,allocatable,intent(out) :: hmat(:,:)
      character(len=72) :: line
      real*8  :: helem
      integer :: i,j
      integer :: id,io
      logical :: exist
      nao=0
      nel=0
      id=42
      inquire(file=fname,exist=exist)
      if (.not.exist) call raise('E','File not found')
      open(id,file=fname,status='old')
      do
         read(id,'(a)') line
         if(index(line,'$nat').ne.0) then
            if (allocated(hmat)) deallocate(hmat)
            read(line,'(4x,i68)',iostat=io) nao
            allocate( hmat(nao,nao), stat=io )
         endif
         if(index(line,'$nel').ne.0) read(line,'(4x,i68)',iostat=io) nel
         if(index(line,'$hmat').ne.0) then
            hmat = 0.0d0
            do
               read(id,'(a)') line
               if(index(line,'$').ne.0) exit
               read(line,*,iostat=io) i,j,helem
               if (io.ne.0) call raise('E','Bad input!')
               hmat(i,j) = helem
               hmat(j,i) = helem
            enddo
         endif
         if (io.ne.0) call raise('E','Bad input!')
         if(index(line,'$end').ne.0) exit
      enddo
      close(id)
      if(nao.lt.1) call raise('E','Bad atom number')
      if(nel.lt.1) call raise('E','Bad electron number')
      if(.not.allocated(hmat))call raise('E','Hückel matrix not found')

      end subroutine rdhmat

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      subroutine rdargv(fname,infile,step)
      implicit none
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      character(len=:), allocatable,intent(out) :: fname
      logical,intent(out) :: infile
      integer,intent(out) :: step
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      integer :: i,j,k,l
      integer :: err
      logical :: exist
      logical :: getopts
      logical :: inchrg
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      character(len=:), allocatable :: arg
      character(len=:), allocatable :: sec
      interface
         subroutine rdarg(i,arg)
         integer,intent(in) :: i
         character(len=:),allocatable,intent(out) :: arg
         end subroutine rdarg
         subroutine raise(mode,message)
         character,intent(in) :: mode
         character(len=*),intent(in) :: message
         end subroutine raise
      end interface
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
c     get number of argument and loop over all of them
      step = 6 ! default
      getopts = .true.
      j = 0
      do i = 1, command_argument_count()
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
c        cycle if some command already have read arguments
         if (j.gt.0) then
            j = j-1
            cycle
         endif
         call rdarg(i,arg)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
c        check if there is a '--' present, forbid to read flags
         if (arg.eq.'--') then
            getopts = .false.
            cycle
         endif
         if (getopts) then
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
c           check arguments against select case statement
            select case(arg)
            case('-h','-help', '--help')
               call help
               call terminate(0)
            case('-s','-step', '--step')
               j=1; call rdarg(i+1,sec); read(sec,*,iostat=err) step
               if (err.ne.0) call raise('E','Bad argument!')
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
c           if no flag does fit, assume as file name
            case default
               inquire(file=arg,exist=exist)
               if (exist) then
                  fname = arg
                  infile = .true.
               else
                  if (arg(1:1).eq.'-') then
                     call raise('W','Flag unknown: '//arg)
                  else
                     call raise('E','File not found: '//arg)
                  endif
               endif
            end select
         else
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
c           read all arguments as file names if '--' was encountered
            inquire(file=arg,exist=exist)
            if (exist) then
               fname = arg
            else
               call raise('E','File not found: '//arg)
            endif
         endif
      enddo

      if (.not.allocated(fname)) infile=.false.

      if (allocated(arg)) deallocate(arg)
      if (allocated(sec)) deallocate(sec)
      end subroutine rdargv     

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      subroutine rdarg(i,arg)
      integer,intent(in) :: i
      character(len=:),allocatable,intent(out) :: arg
      integer :: l,err
      if (allocated(arg)) deallocate(arg)
      call get_command_argument(i,length=l,status=err)
      if (err.gt.0) call raise('E','Command argument corrupted')
      allocate( character(len=l) :: arg, stat=err )
      if (err.ne.0) call raise('E','could not be allocated')
      call get_command_argument(i,arg,status=err)
      if (err.gt.0) call raise('E','Command argument corrupted')
      end subroutine rdarg

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      subroutine rdvar(name,var)
      character(len=*),intent(in) :: name
      character(len=:),allocatable,intent(out) :: var
      integer :: l,err
      if (allocated(var)) deallocate(var)
      call get_environment_variable(name,length=l,status=err)
      if (err.gt.0) call raise('E','System variable unassigned')
      allocate( character(len=l) :: var, stat=err )
      if (err.ne.0) call raise('E','could not be allocated')
      call get_environment_variable(name,var,status=err)
      if (err.gt.0) call raise('E','System variable corrupted')
      end subroutine rdvar
