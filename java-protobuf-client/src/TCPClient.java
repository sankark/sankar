import java.io.*;
import java.net.*;

class TCPClient
{
 public static void send(byte[] data) throws Exception
 {
  Socket socket = new Socket("localhost", 2222);
  OutputStream out = socket.getOutputStream(); 
  DataOutputStream dos = new DataOutputStream(out);

  dos.writeInt(data.length);
  if (data.length > 0) {
      dos.write(data, 0, data.length);
  }
  
  
  
  socket.close();
 }
}