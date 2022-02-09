module ordenar

    contains 
!_________________________________________________________________________________________________
    subroutine ord_intercambio(X,n)
        integer :: i, j, aux
        integer :: X(n)

        X=(/2,0,6,5,3,1,8,4/)

        do i=1,n                     !Recorremos la lista de numeros
            aux=X(i)                 !Empezamos con el primer elemento
            do j=1,n 
                if(aux<X(j)) then    !Comprueba el primer numero de la lista con los demas
                    aux=X(i)
                    X(i)=X(j)        !Si el numero de la izquierda es mayor, se permutan
                    X(j)=aux
                end if
            end do
            write(unit=10, fmt='(8(I4))', iostat=ierr) X
        end do
    end subroutine ord_intercambio
!_________________________________________________________________________________________________
    subroutine ord_seleccion(X,n) 
        integer :: i, j, aux, min
        integer :: X(n)

        X=(/2,0,6,5,3,1,8,4/)

        do i=1,n-1       !Recorremos todas las posiciones
            min=i        !Tomamos como minimo la primera posicion
            do j=i+1,n   !Buscamos el minimo a la derecha de i
                if (X(min)>X(j)) then 
                    min=j
                end if   !Se cambian las posiciones para que el minimo se quede a la izquierda
            end do
            aux=X(i)
            X(i)=X(min)  
            X(min)=aux 
            write(unit=10, fmt='(8(I4))', iostat=ierr) X
        end do
    end subroutine ord_seleccion
!_________________________________________________________________________________________________
    subroutine ord_insercion(X,n)
        integer :: i, j, aux
        integer :: X(n)

        do i=2,n         !Recorremos la lista empezando por el segundo elemento
            aux=X(i)     !Guardamos ese valor
            j=i-1
            do while (X(j)>aux.and.j>0) !Lo comparamos
                X(j+1)=X(j)
                j=j-1    !Si es menor lo intercambiamos
                if (j==0)exit 
            end do
            X(j+1)=aux 
            write(unit=10, fmt='(8(I4))', iostat=ierr) X
        end do
    end subroutine ord_insercion 
!_________________________________________________________________________________________________
    subroutine ord_burbuja(X,n) 
        integer :: i, j, aux
        integer :: X(n)

        X=(/2,0,6,5,3,1,8,4/)

        do i=1,n-1                   !Salen combinaciones de todos los elementos      
            do j=i+1,n               !ordenados por parejas
                if (X(i)>X(j)) then  
                    aux=X(i)
                    X(i)=X(j)        !Si el numero de la izquierda es mayor, cambia uno por otro
                    X(j)=aux
                end if
            end do
            write(unit=10, fmt='(8(I4))', iostat=ierr) X
        end do
    end subroutine ord_burbuja

end module ordenar