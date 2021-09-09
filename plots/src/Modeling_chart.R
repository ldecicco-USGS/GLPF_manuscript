library("DiagrammeR")

grViz(" 
      digraph CFA {

# use html formatting for customizing text. Info at this link: http://www.graphviz.org/doc/info/shapes.html#html

      # Header
      node [shape = rectangle, color=CornflowerBlue]
      Header [label = <<b><font color='blue' point-size= '16'>Modeling Process</font><br/></b>> ]; 

      # Watershed model type nodes
      node [shape = box, color = CornflowerBlue] 
      MKE_model [label = <<font color='forestgreen'><b>Single Watershed</b></font><br/>Milwaukee River>];
      Multi_site_models [label = <<font color='forestgreen'><b>Multi-Site Models</b></font><br/>>];
      {rank = same; MKE_model;Multi_site_models}
 
      # Modeling method
      node [shape = ellipse, color=CornflowerBlue]
      ols [label = <<font color='forestgreen'><b>Ordinary Least Squares Model</b></font><br/>>]; 
      lmer [label = <<font color='forestgreen'><b>Linear Mixed Effects Models</b></font><br/>>]; 
      {rank = same; ols;lmer}

      # Sensor vs alternative
      Sensor_model_ols [label = <<font color='forestgreen'><b>Existing Sensor Signals</b></font><br/>T and F signals>];
      Alt_model_ols [label = <<font color='forestgreen'><b>Alternative Signals</b></font><br/>Signals other than T and F>];
      Sensor_model_lmer [label = <<font color='forestgreen'><b>Existing Sensor Signals</b></font><br/>T and F signals>];
      Alt_model_lmer [label = <<font color='forestgreen'><b>Alternative Signals</b></font><br/>Signals other than T and F>];
      {rank = same; Sensor_model_ols;Alt_model_ols;Sensor_model_lmer;Alt_model_lmer}
 
      #Formulas
      node [shape = polygon, color = Crimson] 
      Formula_ols [label = <<font color='forestgreen'><b>Response Variable ~ Predictor variables + Seasonal Interaction     </b></font><br/>Bacteria ~ Optical signals * seasonal interaction + Turbidity>];
      Formula_lmer [label = <<font color='forestgreen'><b>Response Variable ~ Predictor variables + Seasonal Interaction  </b></font><br/>  Bacteria ~ Optical signals * seasonal interaction + Turbidity + (Site)>];
      {rank = same; Formula_ols;Formula_lmer}
        
     #Cross Validation
      xv_ols [label = <<font color='forestgreen'><b>Cross Validation</b></font><br/>50-repeated 5-fold>];
      xv_lmer [label = <<font color='forestgreen'><b>Cross Validation</b></font><br/>50-repeated 5-fold>];

      # NRMSEP
      node [shape = 'egg',color = ' forestgreen']
      NRMSEP_ols [label = <<font color='forestgreen'><b>NRMSEP computation</b></font><br/>>];
      NRMSEP_lmer [label = <<font color='forestgreen'><b>NRMSEP computation</b></font><br/>>];

      #Model selection
      model_select_ols [label = <<font color='forestgreen'><b>Model Selection</b></font><br/>>];
     model_select_ols_2 [label = 'Choose Model with Lowest Median NRMSEP'];
       model_select_lmer [label = <<font color='forestgreen'><b>Model Selection</b></font><br/>>];
      model_select_lmer_1 [label = 'Consider Models within 3% of Lowest Median NRMSEP'];
      model_select_lmer_2 [label = 'Choose Final Model Based on Individual Site Performance'];
      {rank = same; model_select_ols_2;model_select_lmer_2}
      {rank = same; model_select_ols;model_select_lmer}

      # Construct chart 
      Header -> {MKE_model Multi_site_models}
      
      MKE_model -> ols -> {Sensor_model_ols Alt_model_ols} -> Formula_ols -> xv_ols -> NRMSEP_ols -> model_select_ols ->
      model_select_ols_2
      Multi_site_models ->  lmer -> {Sensor_model_lmer Alt_model_lmer} -> Formula_lmer -> xv_lmer -> NRMSEP_lmer -> 
      model_select_lmer -> model_select_lmer_1 -> model_select_lmer_2
       }
      

      ")
