package in.umlaut;

import in.umlaut.maths.Maths;

public class PicksTheorem {

    private static int limit = 101;
    private static int[][] gcds = new int[limit][limit];
    private static boolean[] squares = new boolean[(limit + 1) * (limit + 1) * 4];

    public static void main(String... args){
        init();
        test();
    }
    private static void init() {
        for (int i = 0; i < limit; ++i)
            for (int j = 0; j < limit; ++j)
                gcds[i][j] = Maths.gcd(i, j);

        for (int i = 0; i <= limit * 2; ++i)
            squares[i * i] = true;
    }

    private static void test() {
        int quadsWithSquareNumberOfLatticePoints = 0;

        long start = System.currentTimeMillis();

        for(int a = 1; a < limit; a++)
            for(int b = a; b < limit; b++)
                for(int c = b; c < limit; c++)
                    for(int d = c; d < limit; d++) {
                        quadsWithSquareNumberOfLatticePoints += getNumOfQuadsWithSquareNumOfLattice(new int[]{a, b, c, d});
                        if((a != b) && (b == c) && (c != d)) { // 1223 -> 1232
                            quadsWithSquareNumberOfLatticePoints += getNumOfQuadsWithSquareNumOfLattice(new int[]{a, b, d, c});
                        } else if(b != c) { // 1233 -> 1323
                                            // 1234 -> 1324
                                            // 1123 -> 1213
                            quadsWithSquareNumberOfLatticePoints += getNumOfQuadsWithSquareNumOfLattice(new int[]{a, c, b, d});

                            if ((a < b) && (c < d)) { // 1234 -> 1243
                                quadsWithSquareNumberOfLatticePoints += getNumOfQuadsWithSquareNumOfLattice(new int[]{a, b, d, c});
                            }
                        }
                    }
        long mid = System.currentTimeMillis();
        int ans = 0;
        for (int a = 1; a < limit; ++a)
            for (int b = 1; b < limit; ++b)
                for (int c = 1; c < limit; ++c)
                    for (int d = 1; d < limit; ++d) {
                        ans += getLatticesForCordinates(squares, a, b, c, d);
                    }
        long end = System.currentTimeMillis();

        System.out.println("Total quads with square lattice points = " + quadsWithSquareNumberOfLatticePoints);
        System.out.println("Total quads with square lattice points naive = " + ans);

        System.out.println("Time taken by 1st approach : " + (mid - start));
        System.out.println("Time taken by 2nd approach : " + (end - mid));
    }

    private static int getLatticesForCordinates(boolean[] squares, int a, int b, int c, int d) {
        int A = a * b + b * c + c * d + d * a;
        int B = getBoundaryLatticePoints(new int[]{a,b,c,d});
        int inner = 1 + (A - B) / 2;
        if(squares[inner]) {
            return 1;
        }
        return 0;
    }

    private static boolean isItASquare(int lp) {
        return squares[lp];
    }

    private static int getNumOfQuads(int[] x) {
        int configs = 0;

        // 1111, 2222
        if ((x[0] == x[1]) && (x[1] == x[2]) && (x[2] == x[3]))
            configs = 1;

        // 1212
        else if ((x[0] != x[1]) && (x[2] == x[0]) && (x[3] == x[1]))
            configs = 2;

        else if (
            ((x[0] == x[1]) && (x[2] != x[1]) && (x[2] != x[3]))  ||  // 1123
            ((x[0] != x[1]) && (x[2] != x[1]) && (x[2] == x[3]))  ||  // 1233
            ((x[0] != x[1]) && (x[2] == x[1]) && (x[3] != x[2]))  ||   // 1223
            ((x[0] != x[1]) && (x[2] != x[1]) && (x[2] != x[0]) && (x[3] != x[0]) && (x[3] != x[1])) // 1234
        )
            configs = 8;

        else if (
            ((x[0] != x[1]) && (x[2] == x[0]) && (x[3] != x[1])) ||                         // 1213
                ((x[0] != x[1]) && (x[2] != x[1]) && (x[2] != x[0]) && (x[3] == x[1])) ||   // 1232
                ((x[0] == x[1]) && (x[2] != x[1]) && (x[2] == x[3]))               ||       // 1122

                ((x[0] != x[1]) && (x[2] == x[1]) && (x[3] == x[2])) ||                     // 1222
                ((x[0] == x[1]) && (x[2] == x[1]))                                           // 1112
           )
            configs = 4;

        return configs;
    }


    private static int getNumOfQuadsWithSquareNumOfLattice(int[] arr) {
        int lp = getLatticePointsInsideQuad(arr);

        if(isItASquare(lp)) {
            return getNumOfQuads(arr);
        }
        return 0;
    }

    /**
     * Uses Pick's theorem
     * @param arr
     * @return
     */
    private static int getLatticePointsInsideQuad(int[] arr) {
        return (int)(getArea(arr) + 1 - getBoundaryLatticePoints(arr)/2);
    }


    private static double getArea(int[] arr) {
        return (0.5) * (arr[0] + arr[2]) * (arr[1] + arr[3]);
    }

    private static int getBoundaryLatticePoints(int[] arr) {
        return getNumOfLatticePointsOnLine(arr[0], arr[1])
        + getNumOfLatticePointsOnLine(arr[1], arr[2])
        + getNumOfLatticePointsOnLine(arr[2], arr[3])
        + getNumOfLatticePointsOnLine(arr[3], arr[0]) - 4;
    }

    private static int getNumOfLatticePointsOnLine(int x, int y) {
        return gcds[x][y] + 1;
    }
}
