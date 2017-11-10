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
      subroutine prtime(mode)
      character,intent(in) :: mode
      character(len=:),allocatable :: i
      character(len=8)  :: d
      character(len=10) :: t
      character(len=5)  :: z
      integer :: v(8)
      select case(mode)
      case('S','s')
      i = 'started run on'
      case('E','e')
      i = 'finished run on'
      case default
      i = 'current time:'
      end select
      call date_and_time(d,t,z,v)
      write(*,'(''*''x,a,x,a,''/'',a,''/'',a,x,''at'',x,'//
     .   'a,'':'',a,'':'',a)') i,d(:4),d(5:6),d(7:),t(:2),t(3:4),t(5:)
      end subroutine prtime

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      module timings
      real*4, allocatable :: timings_cpu(:)
      integer,allocatable :: timings_wall(:)
      integer :: timings_max
      contains
      subroutine timing(i)
      call system_clock(timings_wall(i))
      call cpu_time(timings_cpu(i))
      end subroutine
      subroutine prtimings
      character(len=*),parameter :: f10 =
     .   '(2x,a,2x,''cpu-time:'',x,f7.2,x,''seconds'')'
      character(len=*),parameter :: f20 =
     .   '(2x,a,x,''wall-time:'',x,f7.2,x,''seconds'')'
      write(*,f10) 'total',
     .   timings_cpu(timings_max)-timings_cpu(1)
      write(*,f20) 'total',
     .   real(timings_wall(timings_max)-timings_wall(1))/10000.0
      end subroutine prtimings
      end module timings

