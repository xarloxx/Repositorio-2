program principal
    use ordenar
    implicit none
    
    integer :: i, j, aux, ierr, met
    integer, parameter :: n=8
    integer :: X(n)

    X=(/2,0,6,5,3,1,8,4/)

    open(unit=10, file='Ordenar_vector.txt', iostat=ierr) !Abrimos el fichero
    if (ierr/=0) stop 'no es posible abrir el archivo Ordenar_vector.txt'
    
    write(*,*) 'Selecciona el metodo deseado para ordenar el vector'
    write(unit=10, fmt='(8(I4))', iostat=ierr) X
    
    write(*,*) 'Pulsa 1 si quieres la ordenacion por intercambio'
    write(*,*) 'Pulsa 2 si quieres la ordenacion por seleccion'
    write(*,*) 'Pulsa 3 si quieres la ordenacion por insercion'
    write(*,*) 'Pulsa 4 si quieres la ordenacion por burbuja'
    read(*,*) met 
    
    select case(met)

    case (1)
        call ord_intercambio(X,n)
    case (2)
        call ord_seleccion(X,n)
    case (3)
        call ord_insercion(X,n)
    case (4)
        call ord_burbuja(X,n)

    case default
        write(*,*) 'Valor no valido. Reinicie el programa'
    end select

    close(10) !Cerramos el fichero

end program