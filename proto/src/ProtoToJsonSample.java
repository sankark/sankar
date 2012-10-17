import java.io.IOException;
import java.io.StringWriter;

import com.dyuproject.protostuff.JsonIOUtil;
import com.example.tutorial.ProtoTemplate.KnowledgeBase;
import com.example.tutorial.ProtoTemplate.Node;
import com.example.tutorial.ProtoTemplate.Request;
import com.example.tutorial.ProtoTemplate.Response;
import com.example.tutorial.SchemaProtoTemplate;




public class ProtoToJsonSample {
	public String toJson(byte[] protoBytes)
	{
		Response.Builder addressBook = Response.newBuilder();
		
		 StringWriter writer = new StringWriter();
		 try {
			 
			 addressBook.mergeFrom(protoBytes);
			 
			   
			    JsonIOUtil.writeTo(writer, addressBook.build(), SchemaProtoTemplate.Response.WRITE, false);


			    System.out.println(writer.getBuffer().toString());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		 return writer.getBuffer().toString();
	}
	
	
	public Request.Builder testData()
	{
		Request.Builder request = Request.newBuilder();

	    
		Node node=Node.newBuilder().setNodeId("node1").setCpu(50).setHeap(10).build();
	    		   
		KnowledgeBase.Builder kb=KnowledgeBase.newBuilder();
		kb.addNodes(node);
		
		
		request.setKb(kb);
	    
	    return request;
	}
public static void main(String[] args) throws IOException {
	
	    
	Request.Builder request = Request.newBuilder();

    
	Node node=Node.newBuilder().setNodeId("node1").setCpu(50).setHeap(70).build();
	Node node2=Node.newBuilder().setNodeId("node2").setCpu(50).setHeap(70).build();
	Node node3=Node.newBuilder().setNodeId("node3").setCpu(50).setHeap(60).build();
    		   
	KnowledgeBase.Builder kb=KnowledgeBase.newBuilder();
	kb.addNodes(node);
	kb.addNodes(node2);
	kb.addNodes(node3);
	
	
	request.setKb(kb);
	    StringWriter writer = new StringWriter();
	    JsonIOUtil.writeTo(writer, request.build(), SchemaProtoTemplate.Request.WRITE, false);


	    System.out.println(writer.getBuffer().toString());
	
}


}
