Program Main

  Use Data
  Use functions
  Use Evolution_Energie

  implicit none

  Integer, parameter :: N = 10
  Real, dimension(N) :: Energies, Vecteur
  Real :: random

  Call ReadData()

  Energies = NRJ_Aleatoires(N)
  !write(6,*) Energies

  T = Temperature(Energies, R)
  write(6,*) "Temperature : ", T

  Vecteur = Vect_Aleatoire(N)
  !write(6,*) Vecteur

End Program Main
