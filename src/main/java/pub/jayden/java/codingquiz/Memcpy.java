package pub.jayden.java.codingquiz;

/**
 * Created by jaydenuk on 2016. 4. 14..
 */
public class Memcpy {

    public static void memcpy(byte[] v, int dest, int src, int size) {

        if(size <= 0 || size > v.length){
            throw new IllegalArgumentException("Size must be bigger than 0 or less than byte size");
        }
        if(dest > v.length-size || src > v.length - size){
            throw new ArrayIndexOutOfBoundsException("dest/src size out of range");
        }

        System.out.println(" ------ Before : ");
        printByteArray(v);

        if(src > dest){
            for(int i = 0; i < size ; i++){
                v[dest+i] = v[src+i];
            }
        }else{
            for(int i = 0 ; i < size ; i++){
                v[dest+size-i] = v[src+size-i];
            }
        }

        System.out.println(" ------ After : ");
        printByteArray(v);

    }

    public static void printByteArray(byte[] v){
        for(byte b: v){
            System.out.print(b);
            System.out.print(", ");
        }
        System.out.println("");
    }

    public static void main(String[] args) {
        byte[] v =  {1,2,3,4,5,6,7,8,9,10,11,12};
        memcpy(v, 2, 5, 5);
        byte[] w =  {1,2,3,4,5,6,7,8,9,10,11,12};
        memcpy(w, 5, 2, 5);
    }
}
