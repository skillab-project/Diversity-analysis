# =========================================================
# SKILLAB ANALYTICS DASHBOARD
# FINAL ADVANCED XAI + CAREER ANALYTICS VERSION
# =========================================================

# ---------------------------------------------------------
# LIBRARIES
# ---------------------------------------------------------

library(shiny)
library(bs4Dash)
library(plotly)
library(echarts4r)
library(dplyr)
library(DT)
library(highcharter)
library(ggplot2)
library(networkD3)
library(tidyr)
library(scales)

# =========================================================
# GLOBAL DATA
# =========================================================

fit_score <- 0.16
competition <- 78
candidate_tier <- "ELITE"

# =========================================================
# TARGET ROLE FIT
# =========================================================

target_fit <- data.frame(
  Dimension = c(
    "Skills Fit",
    "Knowledge Fit",
    "Experience Fit",
    "Market Readiness"
  ),
  
  Score = c(
    0.12,
    0.10,
    0.18,
    0.22
  )
)

# =========================================================
# ESCO SKILL GAPS
# =========================================================

missing_skills <- data.frame(
  
  Skill = c(
    "Computer Programming",
    "Web Programming",
    "JavaScript",
    "Software Architecture",
    "Software Design",
    "Software Prototype",
    "PHP",
    "C#",
    "TypeScript",
    "Database Design"
  ),
  
  Importance = c(
    0.51,
    0.50,
    0.36,
    0.33,
    0.33,
    0.31,
    0.30,
    0.30,
    0.21,
    0.21
  ),
  
  Priority = c(
    "HIGH",
    "HIGH",
    "HIGH",
    "HIGH",
    "MODERATE",
    "MODERATE",
    "MODERATE",
    "LOW",
    "LOW",
    "LOW"
  )
)

missing_skills <-
  missing_skills %>%
  arrange(desc(Importance))

# =========================================================
# ESCO KNOWLEDGE GAPS
# =========================================================

knowledge_gaps <- data.frame(
  
  Knowledge = c(
    "Agile Development",
    "JavaScript Frameworks",
    "SQL Server",
    "CSS",
    "SQL",
    "Artificial Intelligence",
    "Oracle Database",
    "Android",
    "iOS"
  ),
  
  Importance = c(
    0.36,
    0.35,
    0.33,
    0.25,
    0.20,
    0.19,
    0.16,
    0.15,
    0.12
  ),
  
  Priority = c(
    "HIGH",
    "HIGH",
    "HIGH",
    "MODERATE",
    "MODERATE",
    "MODERATE",
    "LOW",
    "LOW",
    "LOW"
  )
)

knowledge_gaps <-
  knowledge_gaps %>%
  arrange(desc(Importance))

# =========================================================
# LEARNING ROADMAP
# =========================================================

learning_roadmap <- data.frame(
  
  Skill = c(
    "Software Architecture",
    "JavaScript",
    "TypeScript",
    "Cloud Computing",
    "Database Design",
    "AI Engineering"
  ),
  
  Priority = c(
    "HIGH",
    "HIGH",
    "MODERATE",
    "MODERATE",
    "LOW",
    "LOW"
  ),
  
  Recommendation = c(
    "Advanced Software Architecture Bootcamp",
    "Modern JavaScript Specialization",
    "TypeScript for Developers",
    "AWS Cloud Practitioner",
    "Database Systems Design",
    "Introduction to AI Systems"
  )
)

# =========================================================
# CAREER PATHS
# =========================================================

career_transition <- data.frame(
  
  Career = c(
    "Software Architect",
    "Data Engineer",
    "AI Engineer",
    "Cloud Engineer",
    "DevOps Engineer"
  ),
  
  Fit = c(
    89,
    74,
    68,
    71,
    77
  ),
  
  Competition = c(
    82,
    65,
    91,
    58,
    62
  ),
  
  Transferable = c(
    6,
    5,
    4,
    5,
    6
  ),
  
  Missing = c(
    3,
    4,
    6,
    3,
    2
  )
)

# =========================================================
# SHAP VALUES
# =========================================================

shap_values <- data.frame(
  
  Skill = c(
    "Software Architecture",
    "JavaScript",
    "Cloud Computing",
    "Database Design",
    "TypeScript",
    "AI Engineering",
    "SQL",
    "Python",
    "DevOps",
    "System Design"
  ),
  
  SHAP = c(
    0.24,
    0.21,
    0.17,
    0.12,
    0.09,
    0.08,
    0.06,
    0.05,
    0.04,
    0.03
  )
)

shap_values <-
  shap_values %>%
  arrange(desc(SHAP))

# =========================================================
# PARALLEL COORDINATES
# =========================================================

candidate_profiles <- data.frame(
  
  Candidate = c(
    "YOU",
    "Candidate A",
    "Candidate B",
    "Candidate C",
    "Candidate D"
  ),
  
  Programming = c(85,70,60,90,75),
  Cloud = c(70,80,55,60,85),
  AI = c(55,75,40,88,50),
  Databases = c(65,60,90,72,58),
  Architecture = c(80,50,45,91,68)
)

# =========================================================
# RADAR PROFILE
# =========================================================

skills_profile <- data.frame(
  skill = c(
    "Java",
    "Python",
    "C++",
    "MySQL",
    "Cloud",
    "Project Management"
  ),
  
  score = c(
    80,
    85,
    70,
    60,
    55,
    50
  )
)

# =========================================================
# SANKEY NODES
# =========================================================

knowledge_nodes <- c(
  "Software Engineering",
  "Cloud Computing",
  "Database Systems",
  "Artificial Intelligence",
  "Agile Development"
)

occupation_nodes <- c(
  "Software Architect",
  "Software Developer",
  "Cloud Engineer",
  "Data Engineer",
  "AI Engineer"
)

skill_nodes <- c(
  "Java",
  "Python",
  "SQL",
  "System Design",
  "Machine Learning",
  "Cloud Platforms",
  "DevOps",
  "Project Management"
)

all_nodes <- data.frame(
  name = c(
    knowledge_nodes,
    occupation_nodes,
    skill_nodes
  )
)

# =========================================================
# SANKEY LINKS
# =========================================================

links <- data.frame(
  
  source = c(
    0,0,0,
    1,1,
    2,2,
    3,3,
    4,4,4,
    
    5,5,5,
    6,6,6,
    7,7,7,
    8,8,8,
    9,9,9
  ),
  
  target = c(
    5,6,7,
    7,9,
    6,8,
    8,9,
    5,6,8,
    
    10,13,17,
    10,11,12,
    15,16,13,
    11,12,13,
    11,14,15
  ),
  
  value = c(
    10,8,7,
    9,6,
    8,7,
    10,8,
    7,6,5,
    
    8,9,5,
    7,9,8,
    10,8,6,
    9,8,6,
    8,10,7
  )
)

# =========================================================
# PRIORITY COLORS
# =========================================================

priority_colors <- c(
  HIGH = "#dc3545",
  MODERATE = "#ffc107",
  LOW = "#28a745"
)

# =========================================================
# UI
# =========================================================

ui <- bs4DashPage(
  
  title = "SKILLAB Analytics",
  
  header = bs4DashNavbar(),
  
  sidebar = bs4DashSidebar(
    
    skin = "dark",
    
    title = "SKILLAB",
    
    bs4SidebarMenu(
      
      bs4SidebarMenuItem(
        text = "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      
      bs4SidebarMenuItem(
        text = "Occupation Fit",
        tabName = "fit",
        icon = icon("bullseye")
      ),
      
      bs4SidebarMenuItem(
        text = "Competition & Explainability",
        tabName = "competition",
        icon = icon("chart-line")
      ),
      
      bs4SidebarMenuItem(
        text = "Skill Gaps & Upskilling",
        tabName = "skills",
        icon = icon("brain")
      ),
      
      bs4SidebarMenuItem(
        text = "Career Path Explorer",
        tabName = "career",
        icon = icon("route")
      )
    )
  ),
  
  body = bs4DashBody(
    
    bs4TabItems(
      
      # =====================================================
      # OVERVIEW
      # =====================================================
      
      bs4TabItem(
        
        tabName = "overview",
        
        fluidRow(
          
          valueBox(
            value = paste0(competition, "%"),
            subtitle = "Competitive Standing",
            icon = icon("trophy"),
            color = "warning"
          ),
          
          valueBox(
            value = paste0(round(fit_score * 100), "%"),
            subtitle = "Overall Fit Score",
            icon = icon("bullseye"),
            color = "success"
          ),
          
          valueBox(
            value = candidate_tier,
            subtitle = "Candidate Tier",
            icon = icon("crown"),
            color = "purple"
          )
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "Competitiveness Gauge",
            solidHeader = TRUE,
            status = "warning",
            
            highchartOutput("gauge")
          ),
          
          box(
            width = 6,
            title = "Skill Profile",
            solidHeader = TRUE,
            status = "primary",
            
            plotlyOutput("radarPlot")
          )
        )
      ),
      
      # =====================================================
      # OCCUPATION FIT
      # =====================================================
      
      bs4TabItem(
        
        tabName = "fit",
        
        fluidRow(
          
          box(
            width = 6,
            title = "Target Occupation Alignment",
            solidHeader = TRUE,
            status = "primary",
            
            plotlyOutput("targetFitPlot")
          ),
          
          box(
            width = 6,
            title = "Market Readiness",
            solidHeader = TRUE,
            status = "success",
            
            highchartOutput("marketGauge")
          )
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "ESCO Skills Gaps",
            solidHeader = TRUE,
            status = "danger",
            
            DTOutput("missingSkillsTable")
          ),
          
          box(
            width = 6,
            title = "ESCO Knowledge Gaps",
            solidHeader = TRUE,
            status = "warning",
            
            DTOutput("knowledgeTable")
          )
        )
      ),
      
      # =====================================================
      # COMPETITION & XAI
      # =====================================================
      
      bs4TabItem(
        
        tabName = "competition",
        
        fluidRow(
          
          box(
            width = 12,
            title = "Competition Progression",
            solidHeader = TRUE,
            status = "warning",
            
            echarts4rOutput("mountain",
                            height = "400px")
          )
        ),
        
        fluidRow(
          
          box(
            width = 6,
            title = "SHAP Waterfall Explanation",
            solidHeader = TRUE,
            status = "danger",
            
            plotlyOutput("waterfallPlot",
                         height = "500px")
          ),
          
          box(
            width = 6,
            title = "Global SHAP Importance",
            solidHeader = TRUE,
            status = "primary",
            
            plotlyOutput("shapBarPlot",
                         height = "500px")
          )
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Parallel Coordinates Candidate Comparison",
            solidHeader = TRUE,
            status = "success",
            
            plotlyOutput("parallelPlot",
                         height = "600px")
          )
        )
      ),
      
      # =====================================================
      # SKILLS
      # =====================================================
      
      bs4TabItem(
        
        tabName = "skills",
        
        fluidRow(
          
          box(
            width = 6,
            title = "Critical Skill Gaps",
            solidHeader = TRUE,
            status = "danger",
            
            plotlyOutput("skillsPlot")
          ),
          
          box(
            width = 6,
            title = "Knowledge Gaps",
            solidHeader = TRUE,
            status = "info",
            
            plotlyOutput("knowledgePlot")
          )
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Learning Roadmap",
            solidHeader = TRUE,
            status = "success",
            
            DTOutput("roadmapTable")
          )
        )
      ),
      
      # =====================================================
      # CAREER
      # =====================================================
      
      bs4TabItem(
        
        tabName = "career",
        
        fluidRow(
          
          box(
            width = 12,
            title = "Career Opportunity Landscape",
            solidHeader = TRUE,
            status = "primary",
            
            plotlyOutput("careerLandscape",
                         height = "550px")
          )
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Knowledge → Occupation → Transferable Skills",
            solidHeader = TRUE,
            status = "warning",
            
            sankeyNetworkOutput(
              "careerSankey",
              height = "750px"
            )
          )
        ),
        
        fluidRow(
          
          box(
            width = 12,
            title = "Career Transition Analysis",
            solidHeader = TRUE,
            status = "info",
            
            DTOutput("careerTable")
          )
        )
      )
    )
  )
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output) {
  
  # =======================================================
  # GAUGE
  # =======================================================
  
  output$gauge <- renderHighchart({
    
    highchart() %>%
      
      hc_chart(type = "gauge") %>%
      
      hc_add_series(
        data = c(competition)
      ) %>%
      
      hc_yAxis(
        min = 0,
        max = 100
      )
  })
  
  # =======================================================
  # RADAR
  # =======================================================
  
  output$radarPlot <- renderPlotly({
    
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      
      add_trace(
        r = skills_profile$score,
        theta = skills_profile$skill,
        name = 'YOU'
      ) %>%
      
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0,100)
          )
        )
      )
  })
  
  # =======================================================
  # TARGET FIT
  # =======================================================
  
  output$targetFitPlot <- renderPlotly({
    
    target_fit2 <-
      target_fit %>%
      arrange(desc(Score))
    
    plot_ly(
      data = target_fit2,
      x = ~Score,
      y = ~reorder(Dimension, Score),
      type = "bar",
      orientation = "h"
    )
  })
  
  # =======================================================
  # MARKET READINESS
  # =======================================================
  
  output$marketGauge <- renderHighchart({
    
    highchart() %>%
      
      hc_chart(type = "solidgauge") %>%
      
      hc_add_series(data = c(competition)) %>%
      
      hc_yAxis(
        min = 0,
        max = 100
      )
  })
  
  # =======================================================
  # DT TABLES
  # =======================================================
  
  output$missingSkillsTable <- renderDT({
    
    datatable(
      missing_skills,
      rownames = FALSE,
      options = list(pageLength = 10)
    ) %>%
      
      formatStyle(
        "Priority",
        
        target = "cell",
        
        backgroundColor = styleEqual(
          c("HIGH","MODERATE","LOW"),
          c("#dc3545","#ffc107","#28a745")
        ),
        
        color = "white",
        fontWeight = "bold"
      )
  })
  
  output$knowledgeTable <- renderDT({
    
    datatable(
      knowledge_gaps,
      rownames = FALSE,
      options = list(pageLength = 10)
    ) %>%
      
      formatStyle(
        "Priority",
        
        target = "cell",
        
        backgroundColor = styleEqual(
          c("HIGH","MODERATE","LOW"),
          c("#dc3545","#ffc107","#28a745")
        ),
        
        color = "white",
        fontWeight = "bold"
      )
  })
  
  output$roadmapTable <- renderDT({
    
    datatable(
      learning_roadmap,
      rownames = FALSE
    ) %>%
      
      formatStyle(
        "Priority",
        
        target = "cell",
        
        backgroundColor = styleEqual(
          c("HIGH","MODERATE","LOW"),
          c("#dc3545","#ffc107","#28a745")
        ),
        
        color = "white",
        fontWeight = "bold"
      )
  })
  
  output$careerTable <- renderDT({
    datatable(career_transition)
  })
  
  # =======================================================
  # SKILLS BAR
  # =======================================================
  
  output$skillsPlot <- renderPlotly({
    
    plot_ly(
      
      data = missing_skills,
      
      x = ~Importance,
      y = ~reorder(Skill, Importance),
      
      type = "bar",
      orientation = "h",
      
      color = ~Priority,
      
      colors = priority_colors
      
    ) %>%
      
      layout(
        xaxis = list(title = "Importance"),
        yaxis = list(title = "")
      )
  })
  
  # =======================================================
  # KNOWLEDGE BAR
  # =======================================================
  
  output$knowledgePlot <- renderPlotly({
    
    plot_ly(
      
      data = knowledge_gaps,
      
      x = ~Importance,
      y = ~reorder(Knowledge, Importance),
      
      type = "bar",
      orientation = "h",
      
      color = ~Priority,
      
      colors = priority_colors
      
    ) %>%
      
      layout(
        xaxis = list(title = "Importance"),
        yaxis = list(title = "")
      )
  })
  
  # =======================================================
  # MOUNTAIN
  # =======================================================
  
  output$mountain <- renderEcharts4r({
    
    mountain_data <- data.frame(
      x = c(0,20,40,60,80,100),
      y = c(0,10,25,50,75,100)
    )
    
    mountain_data %>%
      
      e_charts(x) %>%
      
      e_line(y,
             smooth = TRUE) %>%
      
      e_area(y)
  })
  
  # =======================================================
  # SHAP WATERFALL
  # =======================================================
  
  output$waterfallPlot <- renderPlotly({
    
    plot_ly(
      type = "waterfall",
      
      measure = c(
        rep("relative",
            nrow(shap_values))
      ),
      
      x = shap_values$Skill,
      
      y = shap_values$SHAP
    )
  })
  
  # =======================================================
  # SHAP BAR
  # =======================================================
  
  output$shapBarPlot <- renderPlotly({
    
    plot_ly(
      
      data = shap_values,
      
      x = ~SHAP,
      y = ~reorder(Skill, SHAP),
      
      type = "bar",
      orientation = "h"
      
    ) %>%
      
      layout(
        xaxis = list(
          title = "Mean |SHAP|"
        )
      )
  })
  
  # =======================================================
  # PARALLEL COORDINATES
  # =======================================================
  
  output$parallelPlot <- renderPlotly({
    
    plot_ly(
      type = 'parcoords',
      
      line = list(
        color = c(1,2,2,2,2),
        colorscale = list(
          c(0,'red'),
          c(1,'lightblue')
        )
      ),
      
      dimensions = list(
        
        list(
          range = c(0,100),
          label = 'Programming',
          values = candidate_profiles$Programming
        ),
        
        list(
          range = c(0,100),
          label = 'Cloud',
          values = candidate_profiles$Cloud
        ),
        
        list(
          range = c(0,100),
          label = 'AI',
          values = candidate_profiles$AI
        ),
        
        list(
          range = c(0,100),
          label = 'Databases',
          values = candidate_profiles$Databases
        ),
        
        list(
          range = c(0,100),
          label = 'Architecture',
          values = candidate_profiles$Architecture
        )
      )
    )
  })
  
  # =======================================================
  # CAREER LANDSCAPE
  # =======================================================
  
  output$careerLandscape <- renderPlotly({
    
    plot_ly(
      
      data = career_transition,
      
      x = ~Competition,
      y = ~Fit,
      
      type = "scatter",
      
      mode = "markers+text",
      
      text = ~Career,
      
      textposition = "top center",
      
      size = ~Transferable,
      
      color = ~Missing,
      
      sizes = c(20,80)
      
    ) %>%
      
      layout(
        
        xaxis = list(
          title = "Market Competition"
        ),
        
        yaxis = list(
          title = "Candidate Fit"
        )
      )
  })
  
  # =======================================================
  # SANKEY
  # =======================================================
  
  output$careerSankey <- renderSankeyNetwork({
    
    sankeyNetwork(
      Links = links,
      Nodes = all_nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      fontSize = 14,
      nodeWidth = 30,
      sinksRight = TRUE
    )
  })
}

# =========================================================
# RUN APP
# =========================================================

shinyApp(ui, server)
