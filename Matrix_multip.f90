subroutine Matrix_multip(M,N,MN,a,b,c)
implicit none
integer                 ::a,b,c
real(4),dimension(a,b)  ::M
real(4),dimension(b,c)  ::N
real(4),dimension(a,c)  ::MN
MN = matmul(M,N)
end
