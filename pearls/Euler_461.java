
/**
 *
 *
 * Almost Pi
 * Problem 461
 * Let f_n(k) = e^(k/n) - 1, for all non-negative integers k.

Remarkably, f_200(6) + f_200(75) + f_200(89) + f_200(226) = 3.141592644529… ≈ pi.

In fact, it is the best approximation of π of the form f_n(a) + f_n(b) + f_n(c) + f_n(d)
* for n = 200.

Let g(n) = a^2 + b^2 + c^2 + d^2 for a, b, c, d that minimize the error:
* | f_n(a) + f_n(b) + f_n(c) + f_n(d) - pi|
(where |x| denotes the absolute value of x).

You are given g(200) = 6^2 + 75^2 + 89^2 + 226^2 = 64658.

Find g(10000).
 *
 *
 */


import java.util.Arrays;

public class Euler_461 {


    static public void solve() {

        final double PI = Math.PI ;

        //final int n = 200; // the 'n' from the problem
        final int n = 10000; // the 'n' from the problem


        // ****  Create first vector of f_n(k).

        // We need maximum of
        // f_n(k) = pi ==> exp^(k/n) = pi+1
        // k <= n*ln(pi_1)
        int kMax = (int) Math.ceil(n*Math.log(PI+1) );

        double[] fnk = new double[kMax];

        // fill it in
        for (int ii=0; ii<kMax ; ++ii)
            fnk[ii] = Math.expm1(ii/(double)n);


        // Just cheking:
        System.out.println("\n\nJust cheking their original numbers: \n\n");
        System.out.println("fnk[6]=" + fnk[6]);
        System.out.println("fnk[75]=" + fnk[75]);
        System.out.println("fnk[89]=" + fnk[89]);
        System.out.println("fnk[226]=" + fnk[226]);
        System.out.println("sum-PI=" + (fnk[6] + fnk[75]+ fnk[89]+ fnk[226] - PI));
        // ****  Create the array of sum of two

        double[] fnk_2 = new double[kMax*(kMax+1)/2];
        // fill it in, only up to relevant values
        int l2=0;
        for (int ii=0; ii<kMax ; ++ii)
            for (int jj=ii; jj<kMax ; ++jj)
                if (fnk[ii]+fnk[jj] <= PI)
                    fnk_2[l2++] = fnk[ii]+fnk[jj];
                else
                    break;

        System.out.println("length of fnk_2 is:" + l2);
        // Sort the array
        Arrays.sort(fnk_2,0,l2);

        // We will look for best match
        double minDist = 999; // that's enough
        double val1_min = 0;
        double val2_min = 0;

        // two pointers, going from each side
        int pTop = l2-1;

        for (int ii=0; ii< pTop ; ++ii) {
            while ( pTop>=0   &&   fnk_2[ii]+fnk_2[pTop] >PI ) pTop--;
            // we went to far: We are BELOW pi
            if (PI - (fnk_2[ii]+fnk_2[pTop]) < minDist) {
                minDist = PI - (fnk_2[ii]+fnk_2[pTop] ) ;
                val1_min = fnk_2[ii];
                val2_min = fnk_2[pTop];
            }
            //Let's check the just ABOVE pi
            if (pTop+1<l2) {
                if ((fnk_2[ii]+fnk_2[pTop+1]) -PI < minDist) {
                    minDist = (fnk_2[ii]+fnk_2[pTop+1] ) - PI ;
                    val1_min = fnk_2[ii];
                    val2_min = fnk_2[pTop+1];
                }
            }
        }

        // Using binary search.
        // Moved to the bottom (moved out)
        // Arrays.binarySearch()



        // Unfold the values to indices
        int a=0,b=0,c=0,d=0;
        for (int ii=0 ; ii<kMax ; ++ii)
            for (int jj=0; jj<kMax ; ++jj) {
                if ( fnk[ii]+fnk[jj] == val1_min) { a=ii ; b=jj;}
                if ( fnk[ii]+fnk[jj] == val2_min) { c=ii ; d=jj;}
            }

        System.out.println("\n\n****** Final result \n\n");
        System.out.println("fnk["+a+"]=" + fnk[a]);
        System.out.println("fnk["+b+"]=" + fnk[b]);
        System.out.println("fnk["+c+"]=" + fnk[c]);
        System.out.println("fnk["+d+"]=" + fnk[d]);
        System.out.println("sum-PI=" + (fnk[a] + fnk[b]+ fnk[c]+ fnk[d] - PI));

        System.out.println("Values: a=" + a + " ; b=" + b + " ; c=" + c + " ; d=" + d   );
        System.out.println("Euler answer: "+ (a*a+b*b+c*c+d*d) );


    }



    public static void main(String[] args){

        long startTime = System.nanoTime();
        solve();
        long elapsedTime = System.nanoTime() - startTime;

        // nano: 10^-9
        // Micro: 10^-6
        // Milli: 10^-3
        System.out.println(" Total time:: "
                + (elapsedTime/1000000 )+ "[milliSecond]");

    }

}
