import net.sf.jasperreports.engine.*;
import java.util.*;

public class Reports {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		JasperReport jasperReport;
		JasperPrint jasperPrint;
		String fileName = "";
		try {
			String queryString = System.getProperty("cgi.query_string");

			HashMap<String, String> params = new HashMap<String, String>();
	
			for (String str : queryString.split("&")) {
				String keyValue[] = str.split("=");
				if (keyValue.length > 1)
					params.put(keyValue[0], keyValue[1]);

			}

			if (params.isEmpty()||!params.containsKey("report")||!params.containsKey("type")) {
				System.out.println("Content-type: text/plain\n");
				System.out.println("Parameters not specified.");
				System.exit(1);
			}
			
			jasperReport = JasperCompileManager.compileReport("reports/" + params
					.get("report")+".jrxml");
			jasperPrint = JasperFillManager.fillReport(jasperReport,
					new HashMap(), new JREmptyDataSource());
			if (params.get("type").compareTo("pdf") == 0) {
				fileName = "tmp/" + UUID.randomUUID() + ".pdf";
				JasperExportManager
						.exportReportToPdfFile(jasperPrint, fileName);
			}
			if (params.get("type").compareTo("html") == 0) {
				fileName = "tmp/" + UUID.randomUUID() + ".html";
				JasperExportManager.exportReportToHtmlFile(jasperPrint,
						fileName);
			}
			System.out.println("HTTP/1.1 301 Moved Permanently\nLocation: /" + fileName + "\n");
			System.out.println("Location: /" + fileName + "\n");
			if (params.get("type").compareTo("pdf") == 0)
				System.out.println("Content-Type: application/pdf");
			if (params.get("type").compareTo("html") == 0)
				System.out.println("Content-Type: text/html");
		} catch (Exception e) {
			System.out.println("Content-Type: text/html\n");
			System.out.println("<br>Error description: " + e.getMessage());
			System.exit(1);
		}
	}

}
