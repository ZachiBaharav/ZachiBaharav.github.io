// Including teacher's notes:

// Create an EMPTY project.
// Then, create a file

// single line comment
/*
multiline comment

*/

/* Running from the comman line
--> Important they locate the directory!!
Then, go to the Operating system and run the file using
$ java -version
$ which javac
$ ls
$ javac HelloTester.java
--You just compiled your first Java application, albeit a simple one,
on OSX. The process of compiling will produce a single file,
called "HelloWorld.class". This file contains Java byte codes,
which are the instructions that the Java Virtual Machine understands.
-- Important note: JVM
$ ls -ls
$ java HelloTester
This command will start a Java Virtual Machine and attempt to
load the class called HelloWorld. Once it loads that class,
it will execute the main method I mentioned earlier.
*/



// Add this part only after the BASIC is done
// input
import java.util.Scanner;
//

public class HelloTester {  // comment

// This is a comment

    public static void main(String[] args)
    {
        System.out.println("Hello World!");

        // Run minimum file and see it works.


        // Now, add the Scanner line. Input.
        Scanner in;    // Declaration of variable: Declares 'in' to be of type Scanner
        String name;

        // create a Scanner object, and assign a value 'keyboard'
        in = new Scanner(System.in);

        System.out.print("Enter your name: ");

        // Read the next String
        name = in.next();

        System.out.println("Hello " + name + "!");

        // When will it NOT work?
        // e.g., first name with a space: John Richard Smith
        // Someone entering a wrong thing (escape sequences).

        // Can use print instead of println

        // Just to show how to read an int
        System.out.print("Enter age: ");

        int age;
        age = in.nextInt();

        System.out.println("You age is " + age);  // <- NOTE: integer is
                    // converted to String here!!  VERY important in the sequel.

        in.close();
    }

}
