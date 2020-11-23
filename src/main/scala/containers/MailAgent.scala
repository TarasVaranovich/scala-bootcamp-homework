package containers

import java.util.Properties

import jakarta.mail.internet.MimeMessage
import jakarta.mail.{Message, Session, Transport}

import scala.util.Try

class MailAgent(smtpHost: String = "127.0.0.1", smtpPort: Int = 25) {

  def sendExampleEmail(subject: String, body: String) = {
    val props = new Properties()
    props.put("mail.debug", "true")
    props.put("mail.smtp.auth", "false")
    props.put("mail.smtp.ehlo", "false")
    props.put("mail.smtp.host", smtpHost)
    props.put("mail.smtp.port", smtpPort)
    val session = Session.getDefaultInstance(props)

    Try {
      val msg = new MimeMessage(session)
      msg.setFrom("me@example.com")
      msg.setRecipients(Message.RecipientType.TO, "you@example.com")
      msg.setSubject(subject)
      msg.setText(s"$body\n")
      Transport.send(msg)
    }.toEither
  }
  //Got bad greeting from SMTP host: 127.0.0.1, port: 25, response: [EOF]
}
