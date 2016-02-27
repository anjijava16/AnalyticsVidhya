

  train= read.csv("train.csv",header=TRUE)
  intern= read.csv("Internship.csv",header=TRUE)
  student= read.csv("Student.csv",header=TRUE)
  
  
  library(data.table)
  
  
  
  #head(train,10) 
  library(plyr)
  
  data<-join(train, intern,by='Internship_ID',
             type = "left" )
  
  
  data<-join(data, student,by='Student_ID',type = "left" )
  nrow(data) 
  areaofInterest<-c('UI',
               'Marketing',
               'Media',
               'Social',
               'Design',
               'Web',
               'Development',
               'Business',
               'Research',
               'Writing',
               'Plan',
               'Creative',
               'Process',
               'Database',
               'Strategy',
               'Designing',
               'Analysis',
               'Facebook',
               'Communication',
               'Rest',
               'Android',
               'Presentation',
               'MediaMarketing',
               'Twitter',
               'SocialMediaMarketing',
               'Operations',
               'Java',
               'Quality',
               'HTML',
               'Blogs',
               'DigitalMarketing',
               'PHP',
               'MarketResearch',
               'Recruitment',
               'Testing',
               'CSS',
               'Planning',
               'API',
               'Editing',
               'ContentWriting',
               'Innovative',
               'LeadGeneration',
               'MobileApp',
               'SQL',
               'Accounts',
               'Reporting',
               'JavaScript',
               'Documentation',
               'iOS',
               'Branding',
               'ACTING',
               'Analytics',
               'Initiative',
               'Advertising',
               'ColdCalling',
               'Sourcing',
               'ERP',
               'NGO',
               'Environment',
               'Networking',
               'Production',
               'MySQL',
               'ISO',
               'MarketingStrategy',
               'Survey',
               'Visio',
               'AppDevelopment',
               'FrontEnd',
               'webdevelopment',
               'Integration',
               'HTML5',
               'jQuery',
               'Server',
               'Coding',
               'MBA',
               'ContentCreation',
               'Reading',
               'B2B',
               'ContentDevelopment',
               'Storm',
               'E-commerce',
               'Databases',
               'Programming',
               'Wordpress',
               'Outreach',
               'NABL',
               'WebDesign',
               'Architecture',
               'WebApplication',
               'Adobe',
               'Scala',
               'UI/UX',
               'Python',
               'Mac',
               'Retail',
               'DigitalMedia',
               'ProductDevelopment',
               'DataCollection',
               'Algorithm',
               'LESS',
               'EmailMarketing',
               'Screening',
               'Bootstrap',
               'Finance',
               'ContentMarketing',
               'CommunicationSkil',
               'Hiring',
               'Negotiation',
               'Administration',
               'CommunicationSkills',
               'CSS3',
               'Infographic',
               'Youtube',
               'CRM',
               'CAD',
               'Infographics',
               'Access',
               'Editorial',
               'ARM',
               'AJAX',
               '.NET',
               'Co-ordination',
               'Ownership',
               'Algorithms',
               'Node',
               'Drafting',
               'Blogging',
               'Animation',
               'Teaching',
               'Blogger',
               'RelationshipManagement',
               '3d',
               'HTTP',
               'PressRelease',
               'Accounting',
               'AndroidAppDevelopment',
               'AdobePhotoshop',
               'Photography',
               'SoftwareDevelopment',
               'SocialNetworking',
               'AngularJS',
               'AWS',
               'SecondaryResearch',
               'Recruiting',
               'ClientServicing',
               'Leadership',
               'ContentWriter',
               'WebServices',
               'Payroll',
               'Prospecting',
               'GraphicDesigning',
               'Proofreading',
               'DataEntry',
               'Flex',
               'Creativity',
               'DataManagement',
               'Convincing',
               'GATE',
               'SocialMediaManagement',
               'MachineLearning',
               'ClientRelations',
               'WebApplications',
               'XML',
               'MVC',
               'HTML/CSS',
               'Google+',
               'Typing',
               'Sketch',
               'UIDesign',
               'VisualDesign',
               'CreativeWriting',
               'GraphicDesigner',
               'ProductDesign',
               'PERL',
               'Hindi',
               'Chef',
               'SalesProcess',
               'ASP.NET',
               'Django',
               'PublicRelations',
               'CMS',
               'VendorManagement',
               'ContentStrategy',
               'ClientRelationship',
               'CreativeDesign',
               'C#',
               'JSON',
               'Linux',
               'ClientInteraction',
               'Manufacturing',
               'CustomerRelationshipManagement',
               'RecruitmentProcess',
               'BusinessRelation',
               'TalentAcquisition',
               'CorelDRAW',
               'BigData',
               'MaterialDesign',
               'MarketAnalysis',
               'AdobeIllustrator',
               'RESTAPI',
               'Tally',
               'Electronics',
               'Bee',
               'C++',
               'OnlineResearch',
               'Mockups',
               'FrontEndDevelopment',
               'Gif',
               'ProductManagement',
               'MongoDB',
               'PrimaryResearch',
               'Healthcare',
               'DataAnalytics',
               'GoogleAnalytics',
               'CorePHP',
               'B2BSales',
               'SocialMediaTools',
               'Node.js',
               'Ruby',
               'Drawing',
               'BrandPromotion',
               'Mechanical',
               'Automobile',
               'Lifestyle',
               'WritingBlogs',
               'CodeIgniter',
               'WritingSkills',
               'SQLServer',
               'LogoDesign',
               'ProjectManagement',
               'APIIntegration',
               'ClientCommunication',
               'GrowthHacking',
               'InteriorDesign',
               'Personality',
               'SAP',
               'Scripting',
               'AndroidApplicationDevelopment',
               'EventManagement',
               'BlogWriting',
               'Statistics',
               'Typography'
  )
  
 
 
  to.string <- function(x) {
    string <- x[1]
    for(i in 2:length(x)) {
      string <- paste(string, x[i], sep=",")
    }
    return(string)
  }
   
  
   

totRow <-nrow(intern)
totCol<-length(areaofInterest) 
internIdMain <-c()
interestMain<-c()
countMain<-c()
row<-1
interest<-''
while(row<=totRow )  {
  col<-1  
  totcount<-0
   while(col<=totCol){
    colname<-areaofInterest[col]
    data<-intern[row, colname]
      if( !is.null(data) ){
         if(data==1){
              interest<- paste(colname,interest , sep=",",collapse = '')
              totcount<-totcount+1
        } 
      } 
     
    col<- col+1
  }
  print(intern[row,1])
  internIdMain[row]=intern[row,1]
  result = substr(interest, 1, nchar(interest)-1)
  interestMain[row]=result
  print(totcount)
  countMain[row]=totcount
  interest<-''
  row<- row+1 
}
print(interestMain)
 
dt<-data.table(internIdMain,interestMain,countMain)
print(dt)
  #AreaofInterests<-intern[]
  print(length(dt$interestMain))
  combined <- data[c('Student_ID','Internship_ID','Internship_Profile','Degree','hometown','Is_Part_Time'
         ,'Stream',	'Current_year' ,'Earliest_Start_Date','Profile','Location','Start Date','End Date'
          ,'Year_of_graduation','Performance_PG','Performance_UG'
             ,'Performance_12th','Performance_10th','Experience_Type' 
             ,'Expected_Stipend','Is_Shortlisted' )]
  
  nrow(data) 
  
 # cbind(combined,dt)
#  combined
  
 # table(combined$Is_Shortlisted)
# test = write.csv(file = "data1.csv",combined)
  
  
  
