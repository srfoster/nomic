import logo from './logo.svg';
import './App.css';
//import * as babel from '@babel/standalone';
import ReactDOM from 'react-dom';
import React, { useState, useEffect } from 'react';

import * as Mui from '@material-ui/core';
import * as I from '@material-ui/icons';

 import Draggable from 'react-draggable' 

import {UnControlled as CodeMirror} from 'react-codemirror2' 

require('codemirror/mode/scheme/scheme');

//Supporting dynamic component eval
//window.babel = babel
window.React = React
window.useState = useState

window.server_call = (host,server_function,data,cb) =>{
console.log(data);
var allData = {...data }
console.log(allData);
var encoded = encodeURIComponent(JSON.stringify(allData));
console.log(encoded)
fetch(host + server_function + "?data=" + encoded).then((r)=>r.json())
.then((r)=>{
      cb(r)
      })
}


function App (props){
return <Mui.Container><APIExplorer path="/top" domainSpecific={GameController}></APIExplorer></Mui.Container>
}

function GameController (props){
var [script, setScript] = useState("")

var [lastResponse, setLastResponse] = useState(props.wrapper)

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
 },[])

return <div><Mui.Accordion><Mui.AccordionSummary>Game State</Mui.AccordionSummary><Mui.AccordionDetails><GameState gameState={lastResponse.gameState}></GameState></Mui.AccordionDetails></Mui.Accordion><CodeEditor script={""} onChange={(editor, data, value)=>{setScript(value)}}></CodeEditor><Mui.Button onClick={()=>{ 
    window.server_call("http://localhost:8081", 
    props.wrapper.runScript, 
    {script: script}, 
    (r)=>{ 
     console.log("Setting last response..."); 
     setLastResponse(r) 
     }) 
 }}>Run</Mui.Button></div>
}

function GameState (props){
return <Mui.GridList cellHeight="160" cols="3">{props.gameState.things.map((t)=> <ThingCard thing={t}></ThingCard> )}</Mui.GridList>
}

function ThingCard (props){
var [expanded, setExpanded] = useState(false)

var [program, setProgram] = useState(false)

var displayResponse = (r)=>{


  if(r && r.type == "Program"){
   return <div><tt>{r.lang ? r.lang : ""}</tt><CodeEditor script={r.value} onChange={(editor, data, value)=>{setProgram(value)}}></CodeEditor><Mui.Button onClick={()=>{ 
    window.server_call("http://localhost:8081", 
    "/run", 
    {id: r.id, value: program}, 
    (r)=>{ 
     console.log("Running Program"); 
     }) 
 }}>Run</Mui.Button></div>
  }


  if(typeof(r) == "object"){
   return <Mui.Card variant="outlined" style={{backgroundColor: r.color?r.color:"white", margin: 2}}><Mui.CardContent><ThingSummary onClick={()=>setExpanded(!expanded)} thing={r}></ThingSummary>{!expanded ? "" : 
 
 <Mui.Table><Mui.TableBody>{Object.keys(r).map((k)=>{ 
 return  <Mui.TableRow><Mui.TableCell>{k}</Mui.TableCell><Mui.TableCell>{<ThingCard thing={r[k]}/>}</Mui.TableCell></Mui.TableRow> 
 })}</Mui.TableBody></Mui.Table>}</Mui.CardContent></Mui.Card>






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
}

return displayResponse(props.thing)
}

function ThingSummary (props){
return <Mui.Chip label={Array.isArray(props.thing) ? props.thing.length : props.thing.name} onClick={props.onClick}></Mui.Chip>
}

function CodeEditor (props){
var [value, setValue] = useState(props.script.script)

return <div>

<CodeMirror
value={props.script}
options={{
mode: 'scheme',
theme: 'material',
lineNumbers: true
}}
onChange={props.onChange}
/>

</div>
}

function APIExplorer (props){
var [loaded, setLoaded] = useState(false)

var [response, setResponse] = useState({})

useEffect(()=>{
          if(!loaded){
window.server_call("http://localhost:8081",
                   props.path,
                   {},
                   (r)=>{
                   setResponse(r)
                   })
setLoaded(true)
}
          })

return response ? <ObjectExplorer object={response} onApiCall={setResponse} domainSpecific={props.domainSpecific}></ObjectExplorer> : "waiting on response..."
}

function ObjectExplorer (props){
var displayResponse = (r)=>{
  if(r.type){
    if(r.type == "function"){
      return <FunctionViewer wrapper={r} onCall={props.onApiCall}>domainSpecific{props.domainSpecific}</FunctionViewer>
    }
    if(r.type == "argument"){
      return "Arg"
    }
    let DS = props.domainSpecific
    if(DS) return <DS wrapper={r} />
  }

  if(typeof(r) == "object"){
    return Object.keys(r).map((k)=>{
      return <Mui.List><Mui.ListItem><Mui.ListItemIcon><Mui.Chip label={k}></Mui.Chip></Mui.ListItemIcon><Mui.ListItemText><Mui.Box style={{margin: 5, padding: 5}}>{displayResponse(r[k])}</Mui.Box></Mui.ListItemText></Mui.ListItem></Mui.List>
    })
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
}

return displayResponse(props.object)
}

function FunctionViewer (props){
var [result, setResult] = useState()

var [outgoingArgs, setOutgoingArgs] = useState({})

const call = ()=>{
  Object.keys(props.wrapper.arguments || {}).map((k)=>{
    let defaultValue = props.wrapper.arguments[k].defaultValue;
    if(defaultValue && outgoingArgs[k] === undefined)
      outgoingArgs[k] = defaultValue
  })

  window.server_call("http://localhost:8081",
                     props.wrapper.function,
                     outgoingArgs,
                     (r)=>{
                     if(!props.onCall)
                       setResult(r);
                     else
                       props.onCall(r);
                     })
}


const editorForType = (t, onChange) => {
  if(t.argumentType ==="string")
  return <BasicStringEditor value={t.defaultValue} label={t.argumentType} onChange={onChange}></BasicStringEditor>

  if(t.argumentType ==="boolean")
  return <BasicBooleanEditor value={t.defaultValue} label={t.argumentType} onChange={onChange}></BasicBooleanEditor>

  return <div>What's this??</div>
}

return props.wrapper.type == "function" ?
<Mui.Card><Mui.CardContent><Mui.Typography component="p" variant="h5">function: <Mui.Chip style={{marginLeft: 5}} label={props.wrapper.name}></Mui.Chip></Mui.Typography><Mui.List><Mui.ListItem><Mui.ListItemIcon><I.Person fontSize="small"></I.Person></Mui.ListItemIcon><Mui.ListItemText>{props.wrapper.userDescription}</Mui.ListItemText></Mui.ListItem><Mui.ListItem><Mui.ListItemIcon><I.Code fontSize="small"></I.Code></Mui.ListItemIcon><Mui.ListItemText>{props.wrapper.devDescription}</Mui.ListItemText></Mui.ListItem></Mui.List><Mui.Button onClick={call} color="primary">Call</Mui.Button>{
props.wrapper.arguments ?
<Mui.TableContainer><Mui.TableBody>{
     Object.keys(props.wrapper.arguments).map((arg)=>
     <Mui.TableRow><Mui.TableCell><Mui.Chip label={arg}></Mui.Chip></Mui.TableCell><Mui.TableCell>{editorForType(props.wrapper.arguments[arg], 
                 (s)=>{ 
                 var newArgs = {...outgoingArgs} 
                 newArgs[arg] = s 
                 setOutgoingArgs(newArgs); 
                 } 
                 )}</Mui.TableCell></Mui.TableRow>
     )}</Mui.TableBody></Mui.TableContainer>
: ""
}{result ? <ObjectExplorer object={result} domainSpecific={props.domainSpecific}></ObjectExplorer> : "" }</Mui.CardContent></Mui.Card>
: <Mui.Chip color="secondary" label={"Not a function: "+ JSON.stringify(props.wrapper)}></Mui.Chip>
}

function BasicBooleanEditor (props){
var [checked, setChecked] = useState(props.value)

return <Mui.Switch checked={checked} onChange={(e)=>{setChecked(!checked);props.onChange(!checked)}}></Mui.Switch>
}

function BasicStringEditor (props){
var [value, setValue] = useState(props.value)

return <Mui.TextField onChange={(e) => {setValue(e.target.value); props.onChange(e.target.value)}} label={props.label} value={value} variant="outlined"></Mui.TextField>
}

export default App;
