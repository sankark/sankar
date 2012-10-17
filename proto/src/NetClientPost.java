import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;

import com.dyuproject.protostuff.JsonIOUtil;
import com.example.tutorial.ProtoTemplate.KnowledgeBase;
import com.example.tutorial.ProtoTemplate.Node;
import com.example.tutorial.ProtoTemplate.Request;
import com.example.tutorial.SchemaProtoTemplate;
 
public class NetClientPost extends Thread {
 public static long start;
	public void run(){
		try {
			
		    // Construct data
		    String data = URLEncoder.encode("request", "UTF-8") + "=" + URLEncoder.encode(getInputJson(), "UTF-8");
		    
		    // Send data
		    URL url = new URL("http://127.0.0.1:8000/api/default/grid");
		    URLConnection conn = url.openConnection();
		    conn.setDoOutput(true);
		    OutputStreamWriter wr = new OutputStreamWriter(conn.getOutputStream());
		    wr.write(data);
		    wr.flush();

		    // Get the response
		    BufferedReader rd = new BufferedReader(new InputStreamReader(conn.getInputStream()));
		    String line;
		    while ((line = rd.readLine()) != null) {
		    	System.out.println(line);
		        // Process line...
		    }
		    wr.close();
		    rd.close();
		    long end=System.currentTimeMillis();
		    System.out.println(end-start);
		} catch (Exception e) {
		}
 
	}
	// http://localhost:8080/RESTfulExample/json/product/post
	public static void main(String[] args) {
		int i=100;
		start=System.currentTimeMillis();
		System.out.println(start);
		while(i!=0)
		{
		NetClientPost t1=new NetClientPost();
		t1.start();
		i--;
		}
	}

	private String getInputJson() {
		Request.Builder request = Request.newBuilder();

	    
		Node node=Node.newBuilder().setNodeId("node1").setCpu(50).setHeap(70).build();
		Node node2=Node.newBuilder().setNodeId("node2").setCpu(50).setHeap(60).build();
		Node node3=Node.newBuilder().setNodeId("node3").setCpu(50).setHeap(50).build();
		Node node4=Node.newBuilder().setNodeId("node4").setCpu(50).setHeap(70).build();
		Node node5=Node.newBuilder().setNodeId("node5").setCpu(50).setHeap(60).build();
		Node node6=Node.newBuilder().setNodeId("node6").setCpu(50).setHeap(50).build();
	    		   
		KnowledgeBase.Builder kb=KnowledgeBase.newBuilder();
		kb.addNodes(node);
		kb.addNodes(node2);
		kb.addNodes(node3);
		kb.addNodes(node4);
		kb.addNodes(node5);
		kb.addNodes(node6);
		
		
		request.setKb(kb);
		    StringWriter writer = new StringWriter();
		    try {
				JsonIOUtil.writeTo(writer, request.build(), SchemaProtoTemplate.Request.WRITE, false);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    
		   return writer.getBuffer().toString();

		// TODO Auto-generated method stub
		
	}
 
}