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
      subroutine prmat(mat,d1,d2,name,unit,instep)
      implicit none
      integer,intent(in) :: d1,d2
      real*8, intent(in) :: mat(d1,d2)
      character(len=*),intent(in),optional :: name
      integer,intent(in),optional :: unit
      integer,intent(in),optional :: instep
      integer :: i,j,k,l,step
      if (present(instep)) then
         step = instep
      else
         step = 6
      endif
      if(present(name)) write(*,'(/,''matrix printed:'',x,a)') name
      if(present(unit)) then
         do i = 1, d2, step
            l = min(i+step-1,d2)
            write(unit,'(/,6x,<step>(3x,i4,3x))') (k,k=i,l)
            do j = 1, d1
               write(unit,'(i6,<step>(f10.5))') j,(mat(j,k),k=i,l)
            enddo
         enddo
      else
         do i = 1, d2, step
            l = min(i+step-1,d2)
            write(*,'(/,6x,<step>(3x,i4,3x))') (k,k=i,l)
            do j = 1, d1
               write(*,'(i6,<step>(f10.5))') j,(mat(j,k),k=i,l)
            enddo
         enddo
      endif
      end subroutine prmat
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      subroutine prsmat(mat,d,name,unit,instep)
      implicit none
      integer,intent(in) :: d
      real*8, intent(in) :: mat(d,d)
      character(len=*),intent(in),optional :: name
      integer,intent(in),optional :: unit
      integer,intent(in),optional :: instep
      integer :: i,j,k,l,step
      if (present(instep)) then
         step = instep
      else
         step = 6
      endif
      if(present(name)) write(*,'(/,''matrix printed:'',x,a)') name
      if(present(unit)) then
         do i = 1, d, step
            l = min(i+step-1,d)
            write(unit,'(/,6x,<step>(3x,i4,3x))') (k,k=i,l)
            do j = 1, d
               l = min(i+(step-1),j)
               if(j.lt.i) cycle
               write(unit,'(i6,<step>(f10.5))') j,(mat(j,k),k=i,l)
            enddo
         enddo
      else
         do i = 1, d, step
            l = min(i+step-1,d)
            write(*,'(/,6x,<step>(3x,i4,3x))') (k,k=i,l)
            do j = 1, d
               l = min(i+(step-1),j)
               if(j.lt.i) cycle
               write(*,'(i6,<step>(f10.5))') j,(mat(j,k),k=i,l)
            enddo
         enddo
      endif
      end subroutine prsmat
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      subroutine prrmat(mat,d1,d2,name,unit,instep)
      implicit none
      integer,intent(in) :: d1,d2
      real*8, intent(in) :: mat(d1,d2)
      character(len=*),intent(in),optional :: name
      integer,intent(in),optional :: unit
      integer,intent(in),optional :: instep
      integer :: i,j,k,l,m,step
      if (present(instep)) then
         step = instep
      else
         step = 6
      endif
      if(present(name)) write(*,'(/,''matrix printed:'',x,a)') name
      if(present(unit)) then
         j = 1
         do i = d1, 1, -step
            l = max(i-(step-1),1)
            write(unit,'(/,6x,<step>(3x,i4,3x))') (k,k=j,d1-l+1)
            do m = 1, d2
               write(unit,'(i6,<step>(f10.5))') m,(mat(m,k),k=i,l,-1)
            enddo
            j = j + step
         enddo
         write(unit,'(a)')
      else
         j = 1
         do i = d1, 1, -step
            l = max(i-(step-1),1)
            write(*,'(/,6x,<step>(3x,i4,3x))') (k,k=j,d1-l+1)
            do m = 1, d2
               write(*,'(i6,<step>(f10.5))') m,(mat(m,k),k=i,l,-1)
            enddo
            j = j + step
         enddo
         write(*,'(a)')
      endif
      end subroutine prrmat
