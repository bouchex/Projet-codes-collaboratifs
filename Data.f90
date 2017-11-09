Module Data

  implicit none

  Real :: R, T, tau, F0, dt
 
CONTAINS

  Subroutine ReadData()

    Character (len = 32) :: file_name

    ! Ouverture fichier data.txt
    open (unit=11,file=Data.txt,action="read",status="old")

      Read (unit=11,fmt=*) R
      Read (unit=11,fmt=*) T
      Read (unit=11,fmt=*) tau
      Read (unit=11,fmt=*) F0
      Read (unit=11,fmt=*) dt


    close(unit=11)                   ! fermeture fu fichier

  End Subroutine

END Module Data
