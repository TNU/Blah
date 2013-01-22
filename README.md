Blah
====

**Blah** is a toy programming language written by Tian Yu Zhang.

It is written from scratch completely by hand, without using any generated code.

Here is a sample of Blah code implementing FizzBuzz:

    to check_a_divides_b with a, b do
        return a = 0 or (b / a) * a = b
    done

    num: 1
    while num < 100 then
        if check_a_divides_b(15, num) then
            writeLine('FizzBuzz')

        else if check_a_divides_b(3, num) then
            writeLine('Fizz')

        else if check_a_divides_b(5, num) then
            writeLine('Buzz')

        otherwise
            writeLine(num)
        fi

        num: num + 1
    repeat

And here is a sample of recursion:

    # calculates the factorial of a number
    to calcFactorial with n do
        if n = 0 then
            return 1
        otherwise
            return n * calcFactorial(n - 1)
        fi
    done

    writeLine(calcFactorial(10))

Blah is very slow and there are probably many bugs, so feel free to improve on it or just tell me how to improve it.


