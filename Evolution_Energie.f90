module Evolution_Energie
  Use Data
  Use functions
  implicit none

contains

  Function Evol(Ep)
    Real, dimension(:) :: Ep
    Real, dimension(size(Ep)) :: Evol, sigma
    Real:: frac, RT, div
    Integer :: i

    frac = dt/tau
    RT = R*T*frac
    div = 1./(1.+2.*frac)
    sigma = Vect_Aleatoire(size(Ep))
    Do i = 1, size(Ep)
      !write(6,*) "Evol : ", Ep(i), " sigma : ", sigma(i)
       Evol(i) = div*(Ep(i)+RT*(1.+sigma(i)**2)+2.*sqrt(RT*Ep(i))*sigma(i))
      !Evol(i) = div*((sqrt(Ep(i))+sqrt(RT)*sigma(i))**2 + RT)
      !write(6,*) "Evol : ", Evol(i)
    End do
  End Function Evol

End Module Evolution_Energie
