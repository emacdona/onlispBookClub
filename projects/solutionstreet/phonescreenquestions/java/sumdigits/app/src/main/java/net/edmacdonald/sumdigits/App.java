package net.edmacdonald.sumdigits;

import java.util.Arrays;

public class App {
    public static void main(String[] args) {
       Arrays
          .asList(
            123_456, 
            500_000, 
            500_300, 
            1_000_000, 
            0
          )
          .stream()
          .forEach(n -> {
             testSumDigits(n);
             System.out.println();
          });
    }

    public static void testSumDigits(int num){
       System.out.println(String.format("Sum of digits of '%s' = %s", num, sumdigits(num)));
    }

    public static int sumdigits(int num){
       // num is constrained by how big we make the initial denominator.
       // let's say it's between zero and a million (inclusive)
       int denominator = 1_000_000;
       if(num < 0 || num > denominator ){
          throw new RuntimeException("Illegal argument.");
       }

       int numerator = num;
       int quotient, remainder;
       int sum = 0;

       do {
          quotient  = numerator / denominator;
          remainder = numerator % denominator;
          sum += quotient;

          /*
          System.out.println(String.format(
                   "%7s / %7s = %1s r %7s",  
                   numerator, denominator, quotient, remainder));
           */

          // Shrink the denominator and prepare to divide the remainder
          denominator = denominator / 10;
          numerator = remainder;
       } while (remainder != 0);

       return sum;
    }
}
