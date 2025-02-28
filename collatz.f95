program collatz
    implicit none
    integer :: start_num, end_num, num, steps
    character(len=20) :: filename
    open(unit=10, file='collatz_steps.txt', status='unknown')

    ! Pobranie zakresu od użytkownika
    print *, 'Podaj początek zakresu:'
    read(*,*) start_num
    print *, 'Podaj koniec zakresu:'
    read(*,*) end_num

    if (start_num < 1 .or. end_num < start_num) then
        print *, 'Niepoprawny zakres!'
        stop
    end if

    ! Przetwarzanie liczb w zakresie
    do num = start_num, end_num
        steps = collatz_steps(num)
        write(10,*) num, steps
    end do

    close(10)
    print *, 'Wyniki zapisano do collatz_steps.txt'
contains

    integer function collatz_steps(n) result(count)
        integer, intent(in) :: n
        integer :: value
        count = 0
        value = n
        
        do while (value /= 1)
            if (mod(value, 2) == 0) then
                value = value / 2
            else
                value = 3 * value + 1
            end if
            count = count + 1
        end do
    end function collatz_steps

end program collatz

