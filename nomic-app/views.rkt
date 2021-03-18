#lang at-exp racket

(require racket-react/client
         racket-react/components/api-explorer
         racket-react/components/code-editor)

(define-component
  ManaConstructCard

  (return
   (GridListTile
    (Accordion
     (AccordionSummary  @~{props.manaConstruct.name} " " @~{props.manaConstruct.mana})
     (AccordionDetails
      (p "Owner: " @~{props.manaConstruct.owner}))))))

(define-component
  PlayerState

  (return
   (div style: @~{{padding:10}}
        (Avatar 'alt: @~{props.player.name}
                'style: @~{{color: "black",
                            backgroundColor: props.isCurrentPlayer?"orange":"gray"}}
                @~{props.player.name[0]})
        
        (GridList 'cellHeight: 160
                  'cols: 3
                  @~{props.player.manaConstructs
 .map((mc)=><ManaConstructCard manaConstruct={mc}/>)}))))

(define-component
  Watcher

  (return
   (span 'style: @~{{padding: 5}}
    (Badge 'badgeContent: @~{props.watcher.mana}
           'color: "primary"
           (Chip 
            'label: @~{props.watcher.id})))))

(define-component
  GameState

  (return
   (div
    ;(Accordion
     ;(AccordionSummary "Watchers")
     ;(AccordionDetails
      @~{props.gameState.watchers.map((w)=><Watcher
 watcher={w}/>)}
      ;))

    (hr)
    
    @~{props.gameState.players.map((p)=><PlayerState
 isCurrentPlayer={props.gameState.currentPlayer.name == p.name}
 player={p}/>)}
    ;(ObjectExplorer 'object: @~{props.gameState})
    )))

(define-component
  ActionLog

  (return
   (div
    (ObjectExplorer 'object: @~{props.actionLog})))
  )

(define-component
  CommandHistory

  (return
   (div
    (ul
     @~{props.commandHistory.map((c)=><li>{c}</li>)})))
  )

(define-component
  GameController
  (useState 'script @js{"(nexus)"})
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
    (Accordion
     (AccordionSummary "Last Server Response")
     (AccordionDetails
      (div 
       (ObjectExplorer
        'object: @~{lastResponse}))))

    (Accordion
     (AccordionSummary "Game State")
     (AccordionDetails
      (ChangePlayerButton 'gameState: @~{lastResponse.gameState})
      (GameState 'gameState: @~{lastResponse.gameState})))

    (Accordion
     (AccordionSummary "Action Log")
     (AccordionDetails
      (ActionLog 'actionLog: @~{lastResponse.actionLog || []})))

    (Accordion
     (AccordionSummary "Command History")
     (AccordionDetails
      (CommandHistory 'commandHistory: @~{lastResponse.commandHistory || []})))
    
    (CodeEditor
     'script: @~{"(nexus)"}
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

(define-component ChangePlayerButton
  (return
   (Button 'onClick:
           @~{()=>{
  window.server_call("http://localhost:8081",
  props.gameState.changePlayer,
  {},
  (r)=>{
   console.log("Changed player...");
   //setLastResponse(r)
   })
}}
           "Change Player")))

(define-component App
                  (return
                    (Container
                     ; (GameState)

                      
                      (APIExplorer 'path: "/top"
                                   'domainSpecific:
                                   @~{GameController}
                                   )
                      
                      )))

(displayln (compile-app components))

(save-app)

