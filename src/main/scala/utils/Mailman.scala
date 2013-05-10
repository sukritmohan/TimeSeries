package utils

import javax.mail._
import java.util.Properties
import javax.mail.internet._


/**
 * User: sukrit@quantifind.com
 * Date: 1/25/13
 */
class Mailman (sendFrom: String, sendPwd: String) {

  var SMTP_HOST: String = "smtp.gmail.com"
  var SMTP_PORT: String = 587.toString

  var sendTo: String = _
  var senderName = "QFMailman"

  var emailSubject: String = _
  var emailBody: String = _

  private def props(smtpHost: String, smtpPort: String) = {
    val props = new Properties()

    props.put("mail.smtp.auth", "true")
    props.put("mail.smtp.starttls.enable", "true")
    props.put("mail.smtp.host", smtpHost)
    props.put("mail.smtp.port", smtpPort)

    props
  }

  private def message(props: Properties,
                      sendFrom: String,
                      sendTo: String,
                      sendPwd: String,
                      senderName: String,
                      emailSubject: String) = {

    val session = Session.getInstance(props,
      new Authenticator() {
        override def getPasswordAuthentication = new PasswordAuthentication(sendFrom, sendPwd)
      })

    val message = new MimeMessage(session)

    if (sendFrom != null)
      message.setFrom(new InternetAddress(sendFrom, senderName))
    else throw new Exception("Must supply sender address!!")

    if (sendTo != null)
      message.setRecipients(Message.RecipientType.TO, sendTo)
    else throw new Exception("Must supply receiver address!!")

    if (emailSubject != null)
      message.setSubject(emailSubject)
    else
      message.setSubject("<no_subject>")

    message
  }

  def sendEmail(smtpHost:String=SMTP_HOST,
                smtpPort:String=SMTP_PORT,
                sendTo:String=sendTo,
                sendFrom:String=sendFrom,
                senderName:String=senderName,
                sendPwd:String=sendPwd,
                emailSubject:String=emailSubject,
                emailBody:String=emailBody) = {


    val messageToSend = message(props(smtpHost, smtpPort),
      sendFrom,
      sendTo,
      sendPwd,
      senderName,
      emailSubject)
    messageToSend.setText(emailBody)

    Transport.send(messageToSend)
  }
}
