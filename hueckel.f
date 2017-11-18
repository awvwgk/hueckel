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
      subroutine prmat(mat,d1,d2,name,unit,step)
      integer,intent(in) :: d1,d2
      real*8, intent(in) :: mat(d1,d2)
      character(len=*),intent(in),optional :: name
      integer,intent(in),optional :: unit,step
      end subroutine prmat
      subroutine prsmat(mat,d,name,unit,step)
      integer,intent(in) :: d
      real*8, intent(in) :: mat(d,d)
      character(len=*),intent(in),optional :: name
      integer,intent(in),optional :: unit,step
      end subroutine prsmat
      subroutine prrmat(mat,d1,d2,name,unit,step)
      integer,intent(in) :: d1,d2
      real*8, intent(in) :: mat(d1,d2)
      character(len=*),intent(in),optional :: name
      integer,intent(in),optional :: unit,step
      end subroutine prrmat
      end interface
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      real*8,parameter :: a2kc =    627.50947428 ! Eh → kcal/mol
      real*8,parameter :: a2ev =     27.21138602 ! Eh → eV
      real*8,parameter :: a2kj =   2625.499638   ! Eh → kJ/mol
      real*8,parameter :: a2cm = 219474.6313702  ! Eh → cm⁻¹
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      character(len=:),allocatable :: fname
      character(len=72) :: line
      integer :: nao,nel,nfermi
      integer :: step
      integer :: i,j,k,l,n,m
      integer :: io,id=42,info
      real*8, allocatable :: hmat(:,:),work(:),eigv(:),p(:,:)
      integer,allocatable :: nocc(:)
      real*8  :: helem,e
      logical :: infile
      nel = -1
      nao = -1
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
            if (line.eq.'') then
               write(*,'(''$end'')')
               exit
            endif
            read(line,*,iostat=io) i,j,helem
            if (io.ne.0) call raise('E','Bad input!')
            hmat(i,j) = helem
            hmat(j,i) = helem
         enddo
      endif
      if(nao.lt.0) call raise('E','Number of atoms not specified')
      if(nao.lt.0) call raise('E','Number of electrons not specified')
      if(.not.allocated(hmat)) call raise('E','No Hückel matrix given')
      write(*,'(/,''* Hückel matrix:'')')
      call prsmat(hmat,nao,step=step)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      write(*,'(/,''* solving eigenvalue problem'')')
      allocate(work(4*nao),eigv(nao))
      call dsyev('V','U',nao,hmat,nao,eigv,work,4*nao,info)
      deallocate(work)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      allocate(nocc(nao))
      nfermi = nel/2
      nocc = 0
      nocc(nfermi+1:nao) = 2
      if (mod(nel,2).eq.1) then
         nocc(nao-nfermi) = 1
      elseif (abs(eigv(nao-nfermi)-eigv(nao-nfermi+1)).lt.1.0d-9) then
         nocc(nao-nfermi) = 1
         nocc(nao-nfermi+1) = 1
      endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      write(*,'(/,''* Hückel eigenvalues:'')')
      j = 1
      do i = nao, 1, -step
         l = max(i-(step-1),1)
         write(*,'(/,6x,<step>(3x,i4,3x))') (k,k=j,nao-l+1)
         write(*,'(2x,''occ:'',<step>(5x,f3.1,2x))')
     .        (dble(nocc(k)),k=i,l,-1)
         write(*,'(2x,''ε/β:'',<step>(f10.5))') (eigv(k),k=i,l,-1)
         j = j + step
      enddo
      write(*,'(/,''* Hückel molecular orbitals:'')')
      call prrmat(hmat,nao,nao,step=step)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      write(*,'(/,''* Density matrix (charge/binding order):'')')
      allocate( p(nao,nao) )
      do i = 1, nao
         do j = 1, nao
            do k = 1, nao
               p(i,j) = p(i,j) + nocc(k)*hmat(i,k)*hmat(j,k)
            enddo
            p(j,i) = p(i,j)
         enddo
      enddo
      call prsmat(p,nao,step=step)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      e = 0.0d0
      do i = 1, nao
         e = e+nocc(i)*eigv(i)
      enddo
      write(*,'(/,''* Hückel energy:'',f13.5,''β'')') e
      write(*,'(3x,''per electron:'',f13.5,''β'')') e/nel
      if (nao.ne.nel) then
         write(*,'(3x,''per   π-atom:'',f13.5,''β'')') e/nao
      endif
      write(*,'(/,''* Resonance energy:'',f10.5,''β'')') (e-nao)
      write(*,'(6x,''per electron:'',f10.5,''β'')') (e-nao)/nel
      if (nao.ne.nel) then
         write(*,'(6x,''per   π-atom:'',f10.5,''β'')') (e-nao)/nao
      endif
      write(*,'(a)')


      call prtime('E')
      call timing(timings_max)
      call prtimings
      call terminate(0)
      end program hueckel
