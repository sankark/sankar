import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import com.dyuproject.protostuff.JsonIOUtil;
import com.example.tutorial.ProtoTemplate.KnowledgeBase;
import com.example.tutorial.ProtoTemplate.Node;
import com.example.tutorial.ProtoTemplate.Request;
import com.example.tutorial.SchemaProtoTemplate;





public class JsonToProtoSample {
	public byte[] toByteArray(String jsonString)
	{
		Request.Builder addressBook = Request.newBuilder();
		 try {
			StringWriter writer = new StringWriter();
			 writer.write(jsonString);
			
			    StringReader in=new StringReader(writer.getBuffer().toString());
			   JsonIOUtil.mergeFrom(in, addressBook, SchemaProtoTemplate.Request.MERGE, true);
			  
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		 return addressBook.build().toByteArray();
	}
public static void main(String[] args) throws IOException {
	
	Request.Builder request= Request.newBuilder();
	Node node=Node.newBuilder().setNodeId("node1").setCpu(50).setHeap(10).build();
	KnowledgeBase.Builder kb=KnowledgeBase.newBuilder();
	kb.addNodes(node);
	
	
	request.setKb(kb);
	 StringWriter writer = new StringWriter();
	    JsonIOUtil.writeTo(writer, request.build(), SchemaProtoTemplate.Request.WRITE, false);
	    boolean numeric = true;
	    Request.Builder request2 = Request.newBuilder();
	    StringReader in=new StringReader(writer.getBuffer().toString());
    JsonIOUtil.mergeFrom(in, request2, SchemaProtoTemplate.Request.MERGE, numeric);
    request2.build().toByteArray();
    System.out.println(request2.getKb().getNodes(0));
	    
/*AddressBook.Builder addressBook = AddressBook.newBuilder();

    
    Person john =
    		  Person.newBuilder()
    		    .setId(1234)
    		    .setName("John Doe")
    		    
    		    .addPhone(
    		      Person.PhoneNumber.newBuilder()
    		        .setNumber("555-4321")
    		        .setType(Person.PhoneType.MOBILE))
    		    .build();
    addressBook.addPerson(john);
    addressBook.addPerson(john);

    
	    boolean numeric = true;

	    StringWriter writer = new StringWriter();
	    JsonIOUtil.writeTo(writer, addressBook.build(), SchemaAddressBookProtos.AddressBook.WRITE, false);

	    AddressBook.Builder addressBook2 = AddressBook.newBuilder();
	    StringReader in=new StringReader(writer.getBuffer().toString());
       JsonIOUtil.mergeFrom(in, addressBook2, SchemaAddressBookProtos.AddressBook.MERGE, numeric);
       addressBook2.build().toByteArray();
       System.out.println(addressBook2.getPerson(0).getName());*/
}
}
