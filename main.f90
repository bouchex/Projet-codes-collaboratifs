Program Main

  Use Data
  Use functions
  Use Evolution_Energie

  implicit none

  Integer, parameter :: N = 10
  Integer :: Niter, i
  Real, dimension(N) :: Energies, Vect_sigma
  Real :: random

  Call ReadData()
  Open(1,file='Temperature.dat')

  Energies = NRJ_Aleatoires(N)
  Vect_sigma = Vect_Aleatoire(N)
  Niter = ceiling(tau/dt)
  !write(6,*) Energies
  Do i = 1, Niter
    Energies = Evol(Energies,Vect_sigma)
    T = Temperature(Energies, R)
    write(1,*) T
  End Do


  Close(1)


  !write(6,*) Vecteur

End Program Main
