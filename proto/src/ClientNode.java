import java.io.IOException;
import java.net.UnknownHostException;

import com.ericsson.otp.erlang.*;
import com.example.tutorial.AddressBookProtos.AddressBook.Builder;
 public class ClientNode { public static void main (String[] _args) throws Exception{ 
ClientNode cnode=new ClientNode();
cnode.testSendJson();
 }

public void sendJsonToErlang(String jsonString) throws IOException, UnknownHostException,
		OtpAuthException, OtpErlangExit, OtpErlangRangeException {
	OtpSelf cNode = new OtpSelf("clientnode", "cookie");
 OtpPeer sNode = new OtpPeer("server@ip-0A557163");
 OtpConnection connection = cNode.connect(sNode);
 System.out.println("test");
 OtpErlangString json=new OtpErlangString(jsonString);
 OtpErlangObject[] args = new OtpErlangObject[]{new OtpErlangAtom("toProto"),json,cNode.pid()};
 connection.send("proto", new OtpErlangTuple(args));
 OtpErlangTuple terms = (OtpErlangTuple) connection.receiveRPC();
 OtpErlangBinary protoBytes = (OtpErlangBinary) terms.elementAt(1);
 System.out.println(protoBytes.binaryValue());
} 

public void sendProtoToErlang(byte[] proto) throws IOException, UnknownHostException,
OtpAuthException, OtpErlangExit, OtpErlangRangeException {
OtpSelf cNode = new OtpSelf("clientnode", "cookie");
OtpPeer sNode = new OtpPeer("server@ip-0A557163");
OtpConnection connection = cNode.connect(sNode);
System.out.println("test");
OtpErlangBinary json=new OtpErlangBinary(proto);
OtpErlangObject[] args = new OtpErlangObject[]{new OtpErlangAtom("toJson"),json,cNode.pid()};
connection.send("json", new OtpErlangTuple(args));
OtpErlangTuple terms = (OtpErlangTuple) connection.receiveRPC();
OtpErlangString protoBytes = (OtpErlangString) terms.elementAt(1);
System.out.println(protoBytes.stringValue());
} 
public void testSendJson()
{
	ProtoToJsonSample sample=new ProtoToJsonSample();
	Builder addressBook = sample.testData();
	String json=sample.toJson(addressBook.build().toByteArray());
	try {
		sendJsonToErlang(json);
	} catch (OtpErlangExit e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (OtpErlangRangeException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (UnknownHostException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (OtpAuthException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
}

public void testSendProto()
{
	ProtoToJsonSample sample=new ProtoToJsonSample();
	Builder addressBook = sample.testData();
	byte[] proto=addressBook.build().toByteArray();
	try {
		sendProtoToErlang(proto);
	} catch (OtpErlangExit e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (OtpErlangRangeException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (UnknownHostException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (OtpAuthException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
}
 
 }