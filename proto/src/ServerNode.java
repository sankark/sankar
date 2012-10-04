import com.ericsson.otp.erlang.*;
 public class ServerNode { public static void main (String[] _args) throws Exception{ 
	 OtpSelf sNode = new OtpSelf("servernode", "cookie");
 
 while(true){
	 sNode.publishPort();
 OtpConnection connection = sNode.accept();
 while(true) try { OtpErlangTuple terms = (OtpErlangTuple) connection.receive();
 System.out.println("Received"+terms.toString());
 OtpErlangAtom action = (OtpErlangAtom) terms.elementAt(0);
 if(action.atomValue().equalsIgnoreCase("toProto")){
 OtpErlangString jsonString = (OtpErlangString) terms.elementAt(1);
 JsonToProtoSample jsonToProto = new JsonToProtoSample();
 byte[] protoBytes=jsonToProto.toByteArray(jsonString.stringValue());
 OtpErlangBinary binary=new OtpErlangBinary(protoBytes);
  System.out.println("Received"+jsonString.stringValue());
 OtpErlangPid from = (OtpErlangPid) terms.elementAt(2);
 OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangAtom("result"),binary };
 OtpErlangTuple result=new OtpErlangTuple(args);
 connection.send(from,result);
 }
 if(action.atomValue().equalsIgnoreCase("toJson")){
	 OtpErlangBinary protoBytes = (OtpErlangBinary) terms.elementAt(1);
	 ProtoToJsonSample protoToJson = new ProtoToJsonSample();
	 String jsonString=protoToJson.toJson(protoBytes.binaryValue());
	 OtpErlangPid from = (OtpErlangPid) terms.elementAt(2);
	 OtpErlangObject[] args = new OtpErlangObject[]{ new OtpErlangAtom("result"),new OtpErlangString(jsonString) };
	 OtpErlangTuple result=new OtpErlangTuple(args);
	 connection.send(from,result);
	 }
 }catch(OtpErlangExit e) { break;
 } sNode.unPublishPort();
 connection.close();
 }
 } }