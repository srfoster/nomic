#lang at-exp racket

(require racket-react/client
         racket-react/components/api-explorer
         racket-react/components/code-editor
         racket-react/components/drag-and-drop)

(define-component ThingSummary
  (return
   (Chip
    'label: @~{Array.isArray(props.thing) ? props.thing.length : props.thing.name}
    'onClick: @~{props.onClick}
    )))

(define-component ThingCard
  (useState 'expanded @js{false})

  (useState 'program @js{false})
  
  @js{var displayResponse = (r)=>{


  if(r && r.type == "Program"){
   return @(div
            (tt @~{r.lang ? r.lang : ""})
            (CodeEditor 'script: @~{r.value}
                        'onChange:
                        @~{(editor, data, value)=>{setProgram(value)}})
            (Button 'onClick:
                    @~{()=>{
     window.server_call("http://localhost:8081",
     "/run",
     {id: r.id, value: program},
     (r)=>{
      console.log("Running Program");
      })
   }}
                    "Run"))                            
  }

	  
  if(typeof(r) == "object"){
   return @(Card 'variant: "outlined"
                  'style: @~{{backgroundColor: r.color?r.color:"white", margin: 2}}
                  (CardContent
                   (ThingSummary onClick: @~{()=>setExpanded(!expanded)}
                                 'thing: @~{r})
                   @~{!expanded ? "" :
 
    @(Table
      (TableBody
       @~{Object.keys(r).map((k)=>{
      return @(TableRow (TableCell @~{k})
                        (TableCell @~{<ThingCard thing={r[k]}/>}))
      })}))}
                   ))



   
                      
    
  } 

  if(typeof(r) == "string"){
   return "\"" + r + "\""
  }

  if(typeof(r) == "boolean"){
   return ""+r 
  }

  if(typeof(r) == "number"){
   return ""+r 
  }

  return typeof(r) 
}}

  (return @js{displayResponse(props.thing)}))



(define-component
  GameState

  (return
   (GridList 'cellHeight: 160
             'cols: 3
             @~{props.gameState.things.map((t)=>@(ThingCard 'thing: @~{t}))})))

(define-component
  GameController
  (useState 'script @js{""})
  (useState 'lastResponse @js{props.wrapper})

  @js{
 useEffect(()=>{
  setInterval(()=>{      
   window.server_call("http://localhost:8081",
   props.wrapper.refresh,
   {},
   (r)=>{
    console.log("Refreshing game state...");
    setLastResponse(r)
    })
   },1000) 
  },[])}
  
  (return
                   
   (div
    #;
    (Accordion
     (AccordionSummary "Last Server Response")
     (AccordionDetails
      (div 
       (ObjectExplorer
        'object: @~{lastResponse}))))

    (Accordion
     (AccordionSummary "Game State")
     (AccordionDetails
      (GameState 'gameState: @~{lastResponse.gameState})))

    (CodeEditor
     'script: @~{""}
     'onChange:
     @~{(editor, data, value)=>{setScript(value)}}
     )
    (Button 'onClick:
            @~{()=>{
  window.server_call("http://localhost:8081",
  props.wrapper.runScript,
  {script: script},
  (r)=>{
   console.log("Setting last response...");
   setLastResponse(r)
   })
}}
            "Run")
    )))

(define-component App
  (return
   (Container
    (APIExplorer 'path: "/top"
                 'domainSpecific:
                 @~{GameController}
                 )
    )))



