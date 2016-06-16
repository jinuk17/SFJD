package pub.jayden.java.network;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Created by jaydenuk on 2016. 4. 6..
 */
public class EchoServer extends Thread {

    final ServerSocket serverSocket;
    static final int PORT = 1717;

    public EchoServer() throws IOException {
        setDaemon(true);
        serverSocket = new ServerSocket(PORT);
    }

    @Override
    public void run() {

        while(true) {
            try(final Socket server = serverSocket.accept();) {
                System.out.println("Waiting for client on port " + serverSocket.getLocalPort() + "...");
                System.out.println("Just connected to " + server.getRemoteSocketAddress());
                DataInputStream in = new DataInputStream(server.getInputStream());
                System.out.println(in.readUTF());
                DataOutputStream out = new DataOutputStream(server.getOutputStream());
                out.writeUTF("Thank you for connecting to " + server.getLocalSocketAddress() + "\nGoodbye!");
            } catch (IOException e) {
                e.printStackTrace();
                break;
            }
        }
    }

}
