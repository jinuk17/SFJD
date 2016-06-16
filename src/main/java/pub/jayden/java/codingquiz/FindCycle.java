package pub.jayden.java.codingquiz;

/**
 * Created by jaydenuk on 2016. 4. 14..
 */
public class FindCycle {

    public static void findCycle(boolean[][] graph, int m) {

        int size = graph.length;

        int[][] result = new int[size][size];

        for(int i = 0 ; i <size ; i++){
            for(int j = 0 ; j < size ; j ++){
                if(i == j) {
                    continue;
                }

                if(graph[i][j]) {
                    result[i][0] = j;

                }
            }
        }
    }


    public static void main(String[] args) {
        boolean[][] graph = {{false,true,false,true},{true,false,false,false},
                {true,false,false,false}, {false,false,true,false}};
        findCycle(graph, 3);
    }
}
