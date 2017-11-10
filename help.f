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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1711
      subroutine banner
      write(*,*)
      write(*,'(7xa)')"┌────────"//
     ."─────────"//
     ."─────────"//
     ."──────────┐"
      write(*,'(7xa)')"│               Hückel               │"
      write(*,'(7xa)')"│          ================          │"
      write(*,'(7xa)')"│               awvwgk               │"
      write(*,'(7xa)')"│        11/2017, Version 1.0        │"
      write(*,'(7xa)')"└─────────"//
     ."────────"//
     ."────────"//
     ."───────────┘"
      write(*,*)
      end subroutine banner

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1711
      subroutine help
      print'(''Usage: s-h [input-file]'',/)'

      print'(''[input-file] may be provided in TM format'','// 
     &   'x,''or can be generated interactively'',/)'

      print'(3x,''-s, --step        '','// 
     &     'x,''Print width of matrices and eigenvalues'')'

      print'(3x,''-h, --help        '','// 
     &     'x,''Show this message'',/)'

      end subroutine help
