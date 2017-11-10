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
      program hueckel
      use timings
      implicit none
C - - all interfaces  - - - - - - - - - - - - - - - - - - - - - - - 1711
      interface
      subroutine rdhmat(fname,nao,nel,hmat)
      character(len=*),intent(in) :: fname
      integer,intent(out) :: nao,nel
      real*8,allocatable,intent(out) :: hmat(:,:)
      end subroutine rdhmat
      subroutine rdargv(fname,infile,step)
      character(len=:), allocatable,intent(out) :: fname
      logical,intent(out) :: infile
      integer,intent(out) :: step
      end subroutine rdargv
      end interface
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      real*8,parameter :: a2kc =    627.50947428 ! Eh → kcal/mol
      real*8,parameter :: a2ev =     27.21138602 ! Eh → eV
      real*8,parameter :: a2kj =   2625.499638   ! Eh → kJ/mol
      real*8,parameter :: a2cm = 219474.6313702  ! Eh → cm⁻¹
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      character(len=:),allocatable :: fname
      character(len=72) :: line
      integer :: nao,nel
      integer :: step
      integer :: i,j,k,l,n,m
      integer :: io,id=42,info
      real*8, allocatable :: hmat(:,:),work(:),eigv(:),p(:,:)
      integer,allocatable :: nocc(:)
      real*8  :: helem
      logical :: infile
      call prtime('S')
      timings_max = 2
      allocate( timings_cpu(timings_max),timings_wall(timings_max) ) 
      call timing(1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      call rdargv(fname,infile,step)
      call banner
C = = read Hückel matrix  = = = = = = = = = = = = = = = = = = = = = 1711
      if (infile) then
         write(*,'(''* read Hückel matrix from'',x,a)') fname
         call rdhmat(fname,nao,nel,hmat)
      else
         write(*,'(''* read Hückel matrix from STDIN'')')
         write(*,'(''$nat '')',advance='no')
         read(*,*,iostat=io) nao
         if (io.ne.0) call raise('E','Bad input!')
         write(*,'(''$nel '')',advance='no')
         allocate( hmat(nao,nao) )
         hmat = 0.0d0
         read(*,*,iostat=io) nel
         if (io.ne.0) call raise('E','Bad input!')
         write(*,'(''$hmat # stop with $end'')')
         do
            write(*,'(''   '')',advance='no')
            read(*,'(a)') line
            if (index(line,'$end').ne.0) exit
            read(line,*,iostat=io) i,j,helem
            if (io.ne.0) call raise('E','Bad input!')
            hmat(i,j) = helem
            hmat(j,i) = helem
         enddo
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      write(*,'(/,''* solving eigenvalue problem'')')
      allocate(work(4*nao),eigv(nao))
      call dsyev('V','U',nao,hmat,nao,eigv,work,4*nao,info)
      deallocate(work)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      write(*,'(/,''* Hückel eigenvalues:'')')
      j = 1
      do i = nao, 1, -step
         l = max(i-(step-1),1)
         write(*,'(/,4x,<step>(3x,i4,3x))') (k,k=j,nao-l+1)
         write(*,'(4x,<step>(f10.5))') (eigv(k),k=i,l,-1)
         j = j + step
      enddo
      write(*,'(/,''* Hückel molecular orbitals:'')')
      j = 1
      do i = nao, 1, -step
         l = max(i-(step-1),1)
         write(*,'(/,4x,<step>(3x,i4,3x))') (k,k=j,nao-l+1)
         do n = 1, nao
             write(*,'(i4,<step>(f10.5))') n,(hmat(n,k),k=i,l,-1)
         enddo
         j = j + step
      enddo
      write(*,*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711

      call prtime('E')
      call timing(timings_max)
      call prtimings
      call terminate(0)
      end program hueckel
