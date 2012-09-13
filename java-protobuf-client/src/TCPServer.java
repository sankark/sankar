import java.io.*;
import java.net.*;

import com.example.tutorial.AddressBookProtos.AddressBook;

class TCPServer
{
   public static void main(String argv[]) throws Exception
      {
         String clientSentence;
         String capitalizedSentence;
         ServerSocket welcomeSocket = new ServerSocket(6789);

         while(true)
         {
            Socket socket = welcomeSocket.accept();
            InputStream in = socket.getInputStream();
            DataInputStream dis = new DataInputStream(in);

            int len = dis.readInt();
            byte[] data = new byte[len];
            if (len > 0) {
                dis.readFully(data);
            }
           AddressBook ab= AddressBook.parseFrom(data);
           ListPeople.Print(ab);
         }
      }
}