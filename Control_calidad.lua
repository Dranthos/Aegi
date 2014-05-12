script_name="Control de calidad"
script_description="Control de calidad"
script_author="unanimated, Dranthos"
script_version="2.2.0E"

require "clipboard"

function qc(subs, sel)

sorted=0	mblur=0		layer=0		malf=0		inside=0	comment=0	dialog=0	bloped=0	contr=0	
dis=0		over=0		gap=0		dspace=0	dword=0		outside=0	op=0		ed=0		sign=0
italics=0	lbreak=0	hororifix=0	zeroes=0	badita=0	dotdot=0	comfail=0	oneframe=0	trf=0
zerot=0		halfsek=0	readableh=0	unreadable=0	saurosis=0	dupli=0		negadur=0	empty=0		orgline=0
tdura=0		tlength=0	tcps=0      mon=0 	tarta=0		mont=0 		monn=0 	inter=0 	err=0  interr=0 	report=""	styles=""	misstyles=""	fontlist=""	fontable={} 

  if pressed=="Limpiar QC" then
    for i=1, #subs do
        if subs[i].class=="dialogue" then
            local line=subs[i]
		line.actor=line.actor
		:gsub("%sPero que haces%.%.%.","")
		:gsub("%s%[salto de %d+ms%]","")
		:gsub("%s%[solapamiento de %d+ms%]","")
		:gsub("%s%[duración negativa%]","")
		:gsub("%s%[tiempo cero%]","")
		:gsub("%s%?%[0 time%]","")
		line.effect=line.effect
		:gsub("%s%[etiquetas malformadas%]","")
		:gsub("%s%[etiquetas inconexas%]","")
		:gsub("%s%[etiquetas redundantes%]","")
		:gsub("%s%.%.%.ordénalo por tiempo por favor","")
		:gsub("%s%[doble espacio%]","")
		:gsub("%s%[letra duplicada%]","")
		:gsub("%s%[cursiva mal puesta%]","")
		:gsub("%s%[comentario jodido%]","")
		:gsub("%s%-FALTA DIFUMINADO%-","")
		:gsub("%s%?%[%.%.%]","")
		:gsub("%s%[¿dificil de leer%?]","")
		:gsub("%s%[dificil de leer%]","")
		:gsub("%s%[imposible de leer%]","")
		:gsub("%s%[¡imposible de leer%!%]","")
		:gsub("%s%[¡¡imposible de leer%!%!%]","")
		:gsub("%s%[¡¡NO LO LEE NI DIOS%!%!%]","")
		:gsub("%s%[por debajo de 0%.5s%]","")
		:gsub("%s%[Exclamación sin cerrar%]","")
		:gsub("%s%[monosilabo que NUNCA lleva tilde%]","")
		:gsub("%s%[¿monosilabo sin tilde%?%]","")
		:gsub("%s%[¿monosilabo que no necesita tilde%?%]","")
		:gsub("%s%[tartamudeo no guay%]","")
		:gsub("%s%[comprobar pronombres interrogativos%]","")
		:gsub("%s%[¿interrogativos sin tilde%?%]","")
		:gsub("%s%[Interrogación sin cerrar%]","")
		:gsub("%s%[Interrogación sin abrir%]","")
		:gsub("%s%[Exclamación sin cerrar%]","")
		:gsub("%s%[Exclamación sin abrir%]","")
		:gsub("%s%[Ex%-%.%.%. no existe mamón%]","")
		:gsub("%s%[Super%-%.%.%. no se usa así%]","")
		:gsub("%s%[Adrede va junto, melón%]","")
		:gsub("%s%[El imperativo del verbo ir es 'idos'%]","")
		:gsub("%s%[Exclamación sin cerrar%]","")
		:gsub("%s%[Exclamación sin cerrar%]","")

            subs[i]=line
        end
    end
  end

  if pressed==">QC" then

    -- make list of styles and fonts
    for i=1, #subs do
      if subs[i].class == "style" then
	fname=subs[i].fontname
	fnam=esc(fname)
 	if not fontlist:match(fnam) then fontlist=fontlist..fname.."\n" table.insert(fontable,fname) end
	styles=styles..subs[i].name..", "
	redstyles=styles
      end
    end
    
    if res.distill=="" then distill="xxxxxxxx" else distill=res.distill end

    for x, i in ipairs(sel) do
        local line=subs[i]
        local text=subs[i].text
	local style=line.style
	local effect=line.effect
	local actor=line.actor
	if style:match("Defa") or style:match("Alt") or style:match("[Oo]verlap") or style:match(distill) then def=1 else def=0 end
	if style:match("^OP") or style:match("^ED") then oped=1 else oped=0 end
	
	visible=text:gsub("{\\alpha&HFF&}[^{}]-({[^}]-\\alpha&H)","%1")	:gsub("{\\alpha&HFF&}[^{}]*$","")	:gsub("{[^{}]-}","")
			:gsub("\\[Nn]","*")	:gsub("%s%?%*+%s%?"," ")	:gsub("^%s+","")	:gsub("%s+$","")
	if text:match("{\\alpha&HFF&}") then alfatime=1 else alfatime=0 end
	nocomment=text:gsub("{[^\\}]-}","")
	cleantxt=text:gsub("{[^}]-}","")
	onlytags=nocomment:gsub("}[^{]-{","}{")	:gsub("}[^{]+$","}")	:gsub("^[^{]+$","")
	parenth=onlytags:gsub("[^%(%)]","")		:gsub("%(%(%)%)","")	:gsub("%(%)","")
	start=line.start_time
	endt=line.end_time
	if i<#subs then nextline=subs[i+1] end
	prevline=subs[i-1]
	if prevline.class=="dialogue" then prevcleantxt=prevline.text:gsub("{[^}]-}","") else prevcleantxt="" end
	prevstart=prevline.start_time
	prevend=prevline.end_time
	dura=endt-start
	dur=dura/1000
	char=visible:gsub(" ","")	:gsub("[%.,\"]","")
	linelen=char:len()
	rawcps=(linelen/dur)
	cps=math.ceil(linelen/dur)

	-- check if sorted by time
	if res["sorted"] then
	if prevline.class=="dialogue" and start<prevstart then
	    effect=effect.."...ordénalo por tiempo por favor" sorted=1
	end	end

      if not line.comment and line.effect~="qcd" then
	-- check for blur
	if res["blur"] and def==0 and visible~="" and not text:match("\\blur") and not text:match("\\be") and endt>0 then
		if res.bloped then  
		  effect=effect.."-FALTA DIFUMINADO-" mblur=mblur+1
		  if oped==1 then bloped=bloped+1 end
		else
		  if oped==0 then effect=effect.." -FALTA DIFUMINADO-" mblur=mblur+1 end
		end
	end


-- busca monosilabos que NUNCA llevan tilde
	if res["monosilabosN"] then
	if visible:match("[^%a][Dd]í[^%a]")
	or visible:match("[^%a][Dd]á[^%a]")
	or visible:match("[^%a][Dd]ió[^%a]")
	or visible:match("[^%a][Dd]ío[^%a]")
	or visible:match("[^%a][Vv]í[^%a]")
	or visible:match("[^%a][Vv]á[^%a]")
	or visible:match("[^%a][Vv]ió[^%a]")
	or visible:match("[^%a][Vv]ío[^%a]")
	or visible:match("[^%a][Ff]ué[^%a]")
	or visible:match("[^%a][Ff]úe[^%a]")
	or visible:match("[^%a][Ff]uí[^%a]")
	or visible:match("[^%a][Ff]úi[^%a]")
	or visible:match("[^%a][Nn]í[^%a]")
	or visible:match("[^%a][Tt]í[^%a]")
	or parenth~=""
	then effect=effect.." [monosilabo que NUNCA lleva tilde]" monn=monn+1 end
	end

	-- busca monosilabos sin tilde
	if res["monosilabos"] then
	if visible:match("[^%a][Tt]u[^%a]")
	or visible:match("[^%a][Mm]as[^%a]")
	or visible:match("[^%a][Ss]i[^%a]")
	or visible:match("[^%a][Ee]l[^%a]")
	or visible:match("[^%a][Aa]un[^%a]")
	then effect=effect.." [¿monosilabo sin tilde?]" mon=mon+1 end
	end

	-- busca monosilabos con tilde
	if res["monosilabost"] then
	if visible:match("[^%a][Tt]ú[^%a]")
	or visible:match("[^%a][Mm]ás[^%a]")
	or visible:match("[^%a][Ss]í[^%a]")
	or visible:match("[^%a][Éé]l[^%a]")
	or visible:match("[^%a][Aa]ún[^%a]")
	then effect=effect.." [¿monosilabo que no necesita tilde?]" mont=mont+1 end
	end

	-- Busca tartamudeos
	if res["tartamudeos"] then
	if visible:match("[^%a]%u%-%l")
	then effect=effect.." [tartamudeo no guay]" tarta=tarta+1 end
	end

	-- Busca interrogativas mal puestas
	if res["interrogativas"] then
	if visible:match("¿") then
		if visible:match("%aue")
		or visible:match("%auien")
		or visible:match("Quienes")
		or visible:match("cual%s")
		or visible:match("cuales%s")
		or visible:match("cuant%a%s")
		or visible:match("cuant%as%s")
		or visible:match("cuanto")
		or visible:match("cuy%a")
		or visible:match("cuy%as")
		then effect=effect.." [comprobar pronombres interrogativos]" inter=inter+1 end end
	if visible:match("%aue")
		 or visible:match("%auien")
		 or visible:match("Quienes")
		 or visible:match("cual%s")
		 or visible:match("cuales%s")
		 or visible:match("cuant%a%s")
		 or visible:match("cuant%as%s")
		 or visible:match("cuanto")
		 or visible:match("cuy%a")
		 or visible:match("cuy%as")
		 then effect=effect.." [¿interrogativos sin tilde?]" inter=inter+1 end
	end

	--comprueba apertura y cierre de signos
	
	if res["apertura"] then

	interabiertas=0
	intercerradas=0
	exclaabiertas=0
	exclacerradas=0
		
		for lol in visible:gmatch("%¿") do
			interabiertas=interabiertas+1 end

		for lol in visible:gmatch("%?") do
			intercerradas=intercerradas+1 end

		for lol in visible:gmatch("%¡") do
			exclaabiertas=exclaabiertas+1 end

		for lol in visible:gmatch("%!") do
			exclacerradas=exclacerradas+1 end

		if interabiertas>intercerradas
			then effect=effect.. " [Interrogación sin cerrar]" interr=interr+1 end
		if interabiertas<intercerradas
			then effect=effect.. " [Interrogación sin abrir]" interr=interr+1 end
		if exclaabiertas>exclacerradas
			then effect=effect.. " [Exclamación sin cerrar]" interr=interr+1 end
		if exclaabiertas<exclacerradas
			then effect=effect.. " [Exclamación sin abrir]" interr=interr+1 end

	interabiertas=0
	intercerradas=0
	exclaabiertas=0
	exclacerradas=0

	end	

	-- busca errores varios
	if res["errores"] then

		-- Error de "ex-" y "super-"
		if text:match("[Ee]x%-")
			then effect=effect.." [Ex-... no existe mamón]" err=err+1 end
		if text:match("[Ss]uper%-")
			then effect=effect.." [Super-... no se usa así]" err=err+1 end
		-- Adrede
    	if text:match("[^%a][Aa]%sdrede[^%a]")
      		then effect=effect.." [Adrede va junto, melón]" err=err+1 end
      	-- Idos
      	if text:match("%s[Ii]ros[^%a]")
      		then effect=effect.." [El imperativo del verbo ir es 'idos']" err=err+1 end

	end

	-- busca etiquetas malformadas o inconexas
	if res["malformed"] then
	if text:match("{[^}]-\\\\[^}]-}")
	or text:match("\\}")  
	or text:match("\\blur%.") 
	or text:match("\\bord%.") 
	or text:match("\\shad%.")
	or text:match("\\alpha[^&\\}]")
	or text:match("\\alpha&[^H]")
	or text:match("\\alpha&H%x[^%x]")
	or text:match("\\alpha&H%x%x[^&]")
	or text:match("\\[1234]a[^&\\}]")
	or text:match("\\[1234]a&[^H]")
	or text:match("\\[1234]c[^&\\}]")
	or text:match("\\[1234]%?c&[^H]")
	or text:match("\\[1234]%?c&%x%x%x%x%x%x[^&]")
	or text:match("{\\[^}]*&&[^}]*}")
	or parenth~=""
	then effect=effect.." [etiquetas malformadas]" malf=malf+1 end
	clrfail=0
	for clr in text:gmatch("c&H(%x+)&") do
	if clr:len()~=6 then clrfail=1 end	end
	if clrfail==1 then effect=effect.." [etiqueta de color mal puesta]" malf=malf+1 end
	if text:match("{\\[^}]*}{\\[^}]*}")
	then effect=effect.." [etiquetas inconexas]" dis=dis+1 end
	end

	-- check for overlaps and gaps
	if res["overlap"] then
	if prevline.class=="dialogue" and style:match("Defa") and prevline.style:match("Defa") 
	and text:match("\\an8")==nil and prevline.text:match("\\an8")==nil and prevline.comment==false then
		if start<prevend and prevend-start<500 and endt-prevend~=0 then 
		actor=actor.." [solapamiento de "..prevend-start.."ms]" over=over+1 
			if prevend-start<100 then actor=actor.." volver a timear" end
		end
		if start>prevend and start-prevend<200 then 
		actor=actor.." [salto de "..start-prevend.."ms]" gap=gap+1 
			if start-prevend<100 then actor=actor.." volver a timear" end
		end
		if endt==start and endt>0 and visible~="" then actor=actor.." [tiempo cero]" zerot=zerot+1 end
		if endt<start then actor=actor.." [duración negativa]" negadur=negadur+1 end
	end	end

	-- check dialogue layer
	if res["dlayer"] then
	if def==1 and line.layer==0 then layer=layer+1 
	end	end

	-- check for double spaces in dialogue
	if res["doublespace"] and def==1 then
		if visible:match("%s%s") then effect=effect.." [doble espacio]" dspace=dspace+1 end
	end

	-- check for double words
	if def==1 then
	visible2w=visible.."."
	    for derp in visible2w:gmatch("%s%?([%w%s\']+)[%p]") do
	    derp2=derp:gsub("^[%a\']+","")
		for a,b in derp:gmatch("([%a\']+)%s([%a\']+)") do
		if a==b and not a:match("^%u") then effect=effect.." [letra duplicada]" dword=dword+1 end
		end
		for a,b in derp2:gmatch("([%a\']+)%s([%a\']+)") do
		if a==b and not a:match("^%u") then effect=effect.." [letra duplicada]" dword=dword+1 end
		end
	    end
	end

	-- check for fucked up comments
	if visible:match("[{}]") or text:match("}[^{]-}") or text:match("{[^}]-{") then comfail=comfail+1 effect=effect.." [comentario jodido]" end

	-- check for bad italics - {\i1}   {\i1}
	if res.failita and not text:match("\\r") then
	  itafail=0
	  itl=""
	  for it in text:gmatch("\\i([01]%?)[\\}]") do 
	    if it=="" then styleref=stylechk(subs,line.style)
	      if styleref.italics then it="1" else it="0" end
	    end
	  itl=itl..it end
	  if itl:match("11") or itl:match("00") then itafail=1 end
	  if itafail==1 then effect=effect.." [cursiva mal puesta]" badita=badita+1 end
	end

	-- check readability	(some sentences are much harder to read than others, so don't take this too seriously, but over 25 is probably bad.)
	ll=linelen ra=0
	if res.read and def==1 and dura>50 and alfatime==0 and prevcleantxt~=cleantxt then		-- these could use rephrasing if possible
	  if cps==23 and ll>60 then effect=effect.." [¿dificil de leer?]" ra=1 end
	  if cps>23 and cps<=26 then 
	    if ll>25 and ll<100 then effect=effect.." [¿dificil de leer?]" ra=1 end
	    if ll>=100 then effect=effect.." [dificil de leer]" ra=1 end
	  end
	  if cps>26 and cps<30 and ll<=30 then effect=effect.." [¿dificil de leer?]" ra=1 end
    if cps>26 and cps<30 then
	    if ll>30 and ll<=60 then effect=effect.." [imposible de leer]" ra=2 end
	    if ll>60 then effect=effect.." [¡imposible de leer!]" ra=2 end
	  end
	  if cps>=30 and cps<=35 then 
	    if ll<=30 then effect=effect.." [imposible de leer]" ra=2 end
	    if ll>30 and ll<=60 then effect=effect.." [¡imposible de leer!]" ra=2 end
	    if ll>60 then effect=effect.." [¡¡imposible de leer!!]" ra=2 end
	  end
	  if cps>35 then effect=effect.." [¡¡NO LO LEE NI DIOS!!]" ra=2 end
    
  if ra==1 then readableh=readableh+1 end
  if ra==2 then unreadable=unreadable+1 end
  end

	-- check for double periods
	if def==1 then
	if visible:match("[^%.]%.%.[^%.]") or visible:match("[^%.]%.%.$") then effect=effect.." [..]" dotdot=dotdot+1 end
	end

	-- check for periods/commas inside/outside quotation marks
	if visible:match("[%.%,]\"") then inside=inside+1 end
	if visible:match("\"[%.%,][^%.]") then outside=outside+1 end

	-- check for redundant tags
	if res.redundant then dup=0
	tags1={"blur","be","bord","shad","fs","fsp","fscx","fscy","frz","frx","fry","fax","fay","c","2c","3c","4c","1a","2a","3a","4a","alpha"}
	  for tax in text:gmatch("({\\[^}]-})") do
	    for i=1,#tags1 do
	      tag=tags1[i]
	      if not tax:match("\\t") and tax:match("\\"..tag.."[%d%-&][^}]-\\"..tag.."[%d%-&]") then dup=1 end
	    end
	  end
	if text:match("{\\[^}]-}$") then dup=1 end
	if dup==1 then dupli=dupli+1 effect=effect.." [etiquetas redundantes]" end
	end

	-- lines under 0.5s
	if res.halfsec and def==1 and visible~="" and ll>8 and prevcleantxt~=cleantxt then
	if dura<500 and dura>50 then halfsek=halfsek+1 effect=effect.." [por debajo de 0.5s]" end
	end

	-- Hdr request against jdpsetting
	if text:match("{\\an8\\bord[%d%.]+\\pos%([%d%.%,]*%)}") then actor=" Pero que haces..." end
	
	-- count OP lines
	if style:match("^OP") then op=op+1 end
	
	-- count ED lines
	if style:match("^ED") then ed=ed+1 end
	
	-- count what's probably signs
	if def==0 and oped==0 then sign=sign+1 end 
	
	-- count linebreaks in dialogue
	if res["lbreax"] and def==1 and nocomment:match("\\N") then lbreak=lbreak+1 end
	
	-- count lines with italics
	if res["italix"] and def==1 and text:match("\\i1") then italics=italics+1 end
	
	-- count honorifics
	if res["honorifix"] and def==1 then
		if visible:match("%a%-san[^%a]") or visible:match("%a%-kun[^%a]") or visible:match("%a%-chan[^%a]")
		or visible:match("%a%-sama[^%a]") or visible:match("%a%-se[mn]pai") or visible:match("%a%-dono")
		or visible:match("%a%-sensei") then hororifix=hororifix+1 end
	end
	
	-- count lines with 0 time
	if res["zero"] then
	if endt==start then zeroes=zeroes+1 actor=actor.." [tiempo cero]" end
	end
	
	-- check for missing styles
	sty=esc(style)
	if res.mistyle and not styles:match(sty) and not misstyles:match(sty) then misstyles=misstyles..style..", " end
	
	-- list unused styles
	if res.uselesstyle then --aegisub.log("\nsty "..sty)
	    if redstyles:match("^"..sty..",") or redstyles:match(", "..sty..",") then 
	    redstyles=redstyles:gsub("^"..sty..", ","") redstyles=redstyles:gsub(", "..sty..", ",", ") end
	end
	
	-- collect font names
	if res.fontcheck and text:match("\\fn") then 
	    for fontname in text:gmatch("\\fn([^}\\]+)") do
	    fname=esc(fontname)
	    if not fontlist:match(fname) then fontlist=fontlist..fontname.."\n" table.insert(fontable,fontname) end
	    end
	end

	-- count dialogue lines
	if def==1 then dialog=dialog+1 end
	
	-- count lines lasting 1 frame
	if dura<=50 and dura>0 then oneframe=oneframe+1 end
	
	-- longest dialogue line: characters
	if def==1 and linelen>tlength then tlength=linelen longtext=visible end
	if longtext==nil then longtext="[No hay líneas de diálogo con texto]" tlength=0 end
	
	-- longest dialogue line: duration
	if def==1 and visible~="" and dura>tdura then tdura=dura ldura=dura/1000 longline=visible end
	if longline==nil then longline="[No hay líneas de diálogo con texto]" ldura=0 end
	
	-- dialogue line with highest CPS
	if def==1 and dura>50 and alfatime==0 and prevcleantxt~=cleantxt and rawcps>tcps then tcps=cps highcps=visible cpstime=dura/1000 end
	if highcps==nil then highcps="[No hay líneas de diálogo con texto]" tcps=0 cpstime=0 end
	
	-- lines with transforms
	if text:match("\\t%(") then trf=trf+1 end
	
	-- lines with \org
	if text:match("\\org%(") then orgline=orgline+1 end
	
	-- empty lines
	if text=="" then empty=empty+1 end
	
      end
	
	-- count commented lines
	if line.comment==true then comment=comment+1 end
	
	if line.effect~=effect or line.actor~=actor then saurosis=saurosis+1 end
	line.actor=actor
	line.effect=effect
	line.text=text
        subs[i]=line
	aegisub.progress.title(string.format("Comprobando línea: %d/%d",x,#sel))
    end
    heather(subs)
    if stitle~=nil then report=report.."Nombre del script:	"..stitle.."\n" end
    if video~=nil then report=report.."Video:	"..video.."\n" end
    if colorspace~=nil then report=report.."Espacio de color:	"..colorspace.."\n" end
    if resx~=nil then report=report.."Resolución del script:	"..resx.."x"..resy.."\n\n" end
    exportfonts="" table.sort(fontable)
	for f=1,#fontable do
	exportfonts=exportfonts..fontable[f]..", "
	end
	exportfonts=exportfonts:gsub(", $","")
	redstyles=redstyles:gsub(", $","")
    
    if #sel==1 then  report=report.."Selección: "..#sel.." línea,   "
    else report=report.."Selección: "..#sel.." lineas,   " end
    report=report.."Comentadas: "..comment.."\n"
    report=report.."Diálogo: "..dialog..",   OP: "..op..",   ED: "..ed..",   TS: "..sign.."\n\n"
    if res["lbreax"] then report=report.."Líneas de diálogo con saltos de línea... "..lbreak.."\n" end
    if res["italix"] then report=report.."Líneas de diálogo con cursivas... "..italics.."\n" end
    if res["honorifix"] then report=report.."Número de honorificos encontrado... "..hororifix.."\n" end
    if res["zero"] then report=report.."Líneas con tiempo cero... "..zeroes.."\n" end
    if res["empty"] then report=report.."Líneas vacías... "..empty.."\n" end
    if res["oneframe"] then report=report.."Líneas que duran 1 marco... "..oneframe.."\n" end
    if res["transline"] then report=report.."Líneas con transformaciones... "..trf.."\n" end
    if res["orgline"] then report=report.."Líneas con org \\org... "..orgline.."\n" end
    if res["longtext"] then report=report.."Línea de diálogo más larga:\n \""..longtext.."\" - "..tlength.." characters\n" end
    if res["longline"] then report=report.."Línea de diálogo con mayor duración:\n \""..longline.."\" - "..ldura.."s\n" end
    if res["highcps"] then report=report.."Línea de diálogo con mayor CPS:\n \""..highcps.."\" - "..tcps.." CPS / "..cpstime.."s\n" end
    
    if res["uselesstyle"] and redstyles~="" then report=report.."\nEstilos redundantes (sin usar): "..redstyles.."\n" end
    if res["fontcheck"] then report=report.."\nFuentes usadas ("..#fontable.."): "..exportfonts.."\n" end
    report=report.."\n\n----------------  PROBLEMAS ENCONTRADOS ----------------\n\n"
    if sorted==1 then report=report.."NO ESTÁ ORDENADO POR TIEMPO.\n" end
    if colorspace=="TV.601" then report=report.."EL ESPACIO DE COLOR ES TV.601. ¡Usa TV.709 o RDF inmolará tu cafetera con arena!\n" end
    if misstyles~="" then misstyles=misstyles:gsub(", $","") report=report.."ESTILOS QUE FALTAN: "..misstyles.."\n" end
    if mblur~=0 then report=report.."Líneas que no son de diálogo sin difuminado... "..mblur.."\n" end
    if bloped~=0 then report=report.."Líneas del OP/ED sin difuminado... "..bloped.."\n" end
    if malf~=0 then report=report.."Líneas con etiquetas malformadas... "..malf.."\n" end
    if mon~=0 then report=report.."Líneas con posibles monosilabos sin tilde diacrítica... "..mon.."\n" end
    if mont~=0 then report=report.."Líneas con posibles monosilabos con tilde diacrítica... "..mont.."\n" end
    if monn~=0 then report=report.."Líneas con monosilabos que NUNCA llevan tilde... "..monn.."\n" end
    if tarta~=0 then report=report.."Líneas con tartamudeos chungos... "..tarta.."\n" end
    if inter~=0 then report=report.."Líneas con posibles pronombres interrogativos mal puestos... "..inter.."\n" end
    if err~=0 then report=report.."Errores tontos varios... "..err.."\n" end
    if interr~=0 then report=report.."Signos sin abrir/cerrar... "..interr.."\n" end
    if dis~=0 then report=report.."Líneas con etiquetas inconexas... "..dis.."\n" end
    if dupli~=0 then report=report.."Líneas con etiquetas redundantes... "..dupli.."\n" end
    if over~=0 then report=report.."Líneas con solapamientos sospechosos... "..over.."\n" end
    if gap>9 then gapu="  --  Timer a shit" else gapu="" end
    if gap~=0 then report=report.."Líneas con saltos sospechosos (por debajo de 200ms)... "..gap..gapu.."\n" end
    if zerot~=0 then report=report.."Líneas con texto pero sin duración... "..zerot.."\n" end
    if negadur~=0 then report=report.."Líneas con duración negativa... "..negadur.."\n" end
    if dspace~=0 then report=report.."Líneas de diálogo con espacios dobles... "..dspace.."\n" end
    if dword~=0 then report=report.."Líneas de diálogo con letras dobles... "..dword.."\n" end
    if dotdot~=0 then report=report.."Líneas de diálogo con dos puntos... "..dotdot.."\n" end
    if halfsek~=0 then report=report.."Líneas de diálogo con menos de 0.5s... "..halfsek.."\n" end
    if readableh~=0 then report=report.."Líneas que puede que sean dificiles de leer... "..readableh.."\n" end
    if unreadable>9 then unrdbl="  --  Menuda mierda de timeo" else unrdbl="" end
    if unreadable~=0 then report=report.."Líneas que son imposibles de leer y necesitan ser retimeadas... "..unreadable..unrdbl.."\n" end
    if badita~=0 then report=report.."Líneas con cursivas jodidas... "..badita.."\n" end
    if comfail~=0 then report=report.."Comentarios jodidos... "..comfail.."\n" end
    if inside~=0 and outside~=0 then 
    report=report.."Puntos/comas dentro de comillas... "..inside.."\n"
    report=report.."Puntos/comas fuera de comillas... "..outside.."\n" end
    if saurosis>0 and saurosis<100 then report=report.."Líneas totales con problemas... "..saurosis.."\n" end
    if saurosis>99 and saurosis<500 then report=report.."Líneas totales con problemas... "..saurosis.." -- Algo estás haciendo mal\n" end
    if saurosis>499 then report=report.."Líneas totales con problemas... "..saurosis.." -- ¡VUELVE A ANIMEID POR EL AMOR DE DIOS!\n" end
    if layer~=0 and sign ~=0 and #sel>dialog then report=report.."Puede que los diálogos se solapen con los carteles, eleva su prioridad\n" end
    if sorted==0 and mblur==0 and malf==0 and dis==0 and over==0 and gap==0 and dspace==0 and dotdot==0 and badita==0 and comfail==0 and unreadable==0 and misstyles=="" and colorspace~="TV.601"
    and tarta==0 and monn==0 and interr==0 and err==0 then
    report=report.."\nEnhorabuena, no se han encontrado problemas serios" else
    if saurosis<500 then report=report.."\nArregla los errores y vuelve a intentarlo" end
    if saurosis>499 then report=report.."\nMejor que te dediques al parchís, en serio. Eso o muérete." end
    end
    brcount=0
    for brk in report:gmatch("\n") do brcount=brcount+1 end
    
        reportdialog=
	{{x=0,y=0,width=50,height=1,class="label",label="Texto a exportar:"},
	{x=0,y=1,width=50,height=brcount/2+6,class="textbox",name="copytext",value=report},}
    pressd,rez=aegisub.dialog.display(reportdialog,{"OK","Copiar","Cancelar"},{ok='OK',close='Cancelar'})
    if pressd=="Copiar" then clipboard.set(report) end	if pressd=="Cancelar" then aegisub.cancel() end
  end
end

function stylechk(subs,stylename)
    for i=1, #subs do
        if subs[i].class=="style" then
	    local style=subs[i]
	    if stylename==style.name then
		styleref=style
	    end
	end
    end
    return styleref
end

function heather(subs)
    stitle,video,colorspace,resx,resy=nil
    for i=1, #subs do
        if subs[i].class=="info" then
	    local k=subs[i].key
	    local v=subs[i].value
	    if k=="Title" then stitle=v end
	    if k=="Video File" then video=v end
	    if k=="YCbCr Matrix" then colorspace=v end
	    if k=="PlayResX" then resx=v end
	    if k=="PlayResY" then resy=v end
	end
    end
end

function esc(str)
str=str
:gsub("%%","%%%%")
:gsub("%(","%%%(")
:gsub("%)","%%%)")
:gsub("%[","%%%[")
:gsub("%]","%%%]")
:gsub("%.","%%%.")
:gsub("%*","%%%*")
:gsub("%-","%%%-")
:gsub("%+","%%%+")
:gsub("%%?","%%%%?")
return str
end

function konfig(subs, sel)
	dialog_config=
	{
	{x=1,y=0,width=1,height=1,class="label",label="Nota: los estilos de los dialogos deben coincidir con 'Defa', 'Alt' o:"},
	{x=2,y=0,width=1,height=1,class="edit",name="distill",},
	{x=1,y=1,width=1,height=1,class="label",label="Analisis [solo se aplica a líneas SELECCIONADAS]:"   },
    {x=1,y=2,width=1,height=1,class="checkbox",name="sorted",label="Comprueba que estén ordenadas por tiempo",value=true},
	{x=1,y=3,width=1,height=1,class="checkbox",name="blur",label="Comprueba si falta difuminado en los carteles",value=true},
	{x=1,y=4,width=1,height=1,class="checkbox",name="bloped",label="Comprueba si falta difuminado en OP/ED",value=true},
	
	{x=1,y=5,width=1,height=1,class="checkbox",name="overlap",label="Comprueba si hay superposiciones/saltos/líneas de duración cero",value=true},
	{x=1,y=6,width=1,height=1,class="checkbox",name="malformed",label="Comprueba si hay etiquetas malformadas/inconexas - \\blur.5, \\alphaFF, \\\\",value=true},
	{x=1,y=7,width=1,height=1,class="checkbox",name="doublespace",label="Comprueba si hay dobles letras o espacios en los dialogos",value=true},
	{x=1,y=8,width=1,height=1,class="checkbox",name="read",label="Comprueba si hay líneas dificiles de leer",value=true},
	{x=1,y=9,width=1,height=1,class="checkbox",name="redundant",label="Comprueba si hay etiquetas redundantes",value=true},
	{x=1,y=10,width=1,height=1,class="checkbox",name="failita",label="Comprueba si hay cursivas mal puestas",value=true},
	{x=1,y=11,width=1,height=1,class="checkbox",name="mistyle",label="Comprueba si faltan estilos",value=true},
	{x=1,y=12,width=1,height=1,class="checkbox",name="dlayer",label="Comprueba la capa de dialogo",value=true},
	{x=1,y=13,width=1,height=1,class="checkbox",name="halfsec",label="Comprueba si hay líneas que duren menos de 0.5s",value=true},
	{x=1,y=14,width=1,height=1,class="checkbox",name="monosilabosN",label="Comprueba si hay monosilabos que NUNCA llevan tilde",value=true},
	{x=1,y=15,width=1,height=1,class="checkbox",name="tartamudeos",label="Comprueba si hay tartamudeos inconsistentes",value=true},
	{x=1,y=16,width=1,height=1,class="checkbox",name="errores",label="Comprueba si hay algunos errores tontos comunes",value=true},
	{x=1,y=17,width=1,height=1,class="checkbox",name="apertura",label="Comprueba si hay signos sin cerrar",value=true},

	{x=1,y=18,width=1,height=1,class="label",label="Utilidades medio útiles que spamean la columna de efecto:",value=true},
	{x=1,y=19,width=1,height=1,class="checkbox",name="monosilabos",label="Comprueba si hay posibles monosilabos sin tilde diacrítica",value=false},
	{x=1,y=20,width=1,height=1,class="checkbox",name="monosilabost",label="Comprueba si hay posibles monosilabos con tilde diacrítica",value=false},
	{x=1,y=21,width=1,height=1,class="checkbox",name="interrogativas",label="Comprueba si hay posibles pronombres interrogativos sin tilde",value=false},

	{x=2,y=1,width=2,height=1,class="label",label="Estadisticas inutiles..."},
	{x=2,y=2,width=2,height=1,class="checkbox",name="italix",label="Cuenta el número de líneas con cursivas",value=false},
	{x=2,y=3,width=2,height=1,class="checkbox",name="lbreax",label="Cuenta el número de líneas con salto de línea",value=false},
	{x=2,y=4,width=2,height=1,class="checkbox",name="honorifix",label="Cuenta el número de honorificos (-san, -kun, -chan)",value=false},
	{x=2,y=5,width=2,height=1,class="checkbox",name="zero",label="Cuenta las líneas de duración cero",value=false},
	{x=2,y=6,width=2,height=1,class="checkbox",name="empty",label="Cuenta las líneas vacías",value=false},
	{x=2,y=7,width=2,height=1,class="checkbox",name="oneframe",label="Cuenta las líneas que duran 1 cuadro",value=false},
	{x=2,y=8,width=2,height=1,class="checkbox",name="transline",label="Cuenta las líneas con transformaciones",value=false},
	{x=2,y=9,width=2,height=1,class="checkbox",name="orgline",label="Cuenta las líneas con \\org",value=false},
	{x=2,y=10,width=2,height=1,class="checkbox",name="longtext",label="Línea con el texto más largo",value=true},
	{x=2,y=11,width=2,height=1,class="checkbox",name="longline",label="Línea con la duración más larga",value=true},
	{x=2,y=12,width=2,height=1,class="checkbox",name="highcps",label="Línea con el CPS más alto",value=true},
	
	{x=2,y=14,width=2,height=1,class="checkbox",name="fontcheck",label="Lista las fuentes usadas",value=false},
	{x=2,y=15,width=2,height=1,class="checkbox",name="uselesstyle",label="Lista los estilos que no se usan",value=true},
	
	--{x=1,y=26,width=2,height=1,class="label",label=""},
	{x=1,y=23,width=2,height=1,class="label",label="Este script es para ayudarte a ver fallos. Si lo estás usando como sustituto de un QC eres un idiota."},
	
	}
	buttons={">QC","Limpiar QC","Marcar todos","Quitar todos","Que le den a esta mierda"}
	
	repeat
	    if pressed=="Marcar todos" or pressed=="Quitar todos" then
		for key,val in ipairs(dialog_config) do
		    if val.class=="checkbox" then
			if pressed=="Marcar todos" then val.value=true end
			if pressed=="Quitar todos" then val.value=false end
		    end
		end
	    end
	pressed,res=aegisub.dialog.display(dialog_config,buttons,{ok='>QC',cancel='Que le den a esta mierda'})
	until pressed~="Marcar todos" and pressed~="Quitar todos"
	
	if pressed==">QC" or pressed=="Limpiar QC" then qc(subs, sel) end
	if pressed=="Que le den a esta mierda" then aegisub.cancel() end
end

function kyuusii(subs, sel)
    konfig(subs, sel) 
    aegisub.set_undo_point(script_name)
    return sel
end

aegisub.register_macro(script_name, script_description, kyuusii)