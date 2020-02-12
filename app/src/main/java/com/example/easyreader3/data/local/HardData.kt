package com.example.easyreader3.data.local

import com.example.easyreader3.classes.*

class HardData {
    val names = "Bob,Mike,Anna,Ivan,Nikita".split(",")
    val emails = "inbox.com,mail.ru,gmail.com,yandex.ru,ulstu.ru".split(",")
    val images = listOf(
        "https://sun9-34.userapi.com/c837337/v837337348/40522/hVLXd0zSz4k.jpg",
        "https://sun9-21.userapi.com/c855320/v855320530/1224cc/dzpU3UcD0tg.jpg",
        "https://sun9-21.userapi.com/c205128/v205128364/2fde1/x1DZiUfACzY.jpg",
        "https://sun9-7.userapi.com/c631916/v631916593/a93/glzAr6IeM24.jpg",
        "https://vk.com/images/camera_50.png"


    )

    fun getGlobalDictionary(level: Int = 100): List<Word> {
        var r = ArrayList<Word>()
        var list = ArrayList<String>(
            (
                    "you,I,to,the,a,and,that,it,of,me,what,is,in,this,know,I'm,for,no,have,my,don't,just,not,do,be,on,your,was,we,it's,with,so,but,all,well,are,he,oh,about,right,you're,get," +
                            "here,out,going,like,yeah,if,her,she,can,up,want,think,that's,now,go,him,at,how,got,there,one,did,why,see,come,good,they,really,as,would,look,when,time,will,okay,back,can't,mean,tell,I'll,from,hey,were,he's,could,didn't," +
                            "yes,his,been,or,something,who,because,some,had,then,say,ok,take,an,way,us,little,make,need,gonna,never,we're,too,love,she's,I've,sure,them,more,over,our,sorry,where,what's,let,thing,am,maybe,down,man,has,uh,very,by," +
                            "there's,should,anything,said,much,any,life,even,off,please,doing,thank,give,only,thought,help,two,talk,people,god,still,wait,into,find,nothing,again,things,let's,doesn't,call,told,great,before,better,ever,night,than," +
                            "away,first,believe,other,feel,everything,work,you've,fine,home,after,last,these,day,keep,does,put,around,stop,they're,I'd,guy,long,isn't,always,listen,wanted,Mr,guys,huh,those,big,lot,happened,thanks,won't,trying,kind," +
                            "wrong,through,talking,made,new,being,guess,hi,care,bad,mom,remember,getting,we'll,together,dad,leave,mother,place,understand,wouldn't,actually,hear,baby,nice,father,else,stay,done,wasn't,their,course,might,mind,every,enough," +
                            "try,hell,came,someone,you'll,own,family,whole,another,house,Jack,yourself,idea,ask,best,must,coming,old,looking,woman,hello,which,years,room,money,left,knew,tonight,real,son,hope,name,same,went,um,hmm,happy,pretty,saw,girl," +
                            "sir,show,friend,already,saying,may,next,three,job,problem,minute,found,world,thinking,haven't,heard,honey,matter,myself,couldn't,exactly,having,ah,probably,happen,we've,hurt,boy,both,while,dead,gotta,alone,since,excuse,start," +
                            "kill,hard,you'd,today,car,ready,until,without,whatever,wants,hold,wanna,yet,seen,deal,took,once,gone,called,morning,supposed,friends,head,stuff,most,used,worry,second,part,live,truth,school,face,forget,true,business,each,cause," +
                            "soon,knows,few,telling,wife,who's,use,chance,run,move,anyone,person,bye,J,somebody,Dr,heart,such,miss,married,point,later,making,meet,anyway,many,phone,reason,damn,lost,looks,bring,case,turn,wish,tomorrow,kids,trust,check," +
                            "change,end,late,anymore,five,least,town,aren't,ha,working,year,makes,taking,means,brother,play,hate,ago,says,beautiful,gave,fact,crazy,party,sit,open,afraid,between,important,rest,fun,kid,word,watch,glad,everyone,days,sister," +
                            "minutes,everybody,bit,couple,whoa,either,Mrs,feeling,daughter,wow,gets,asked,under,break,promise,door,set,close,hand,easy,question,doctor,tried,far,walk,needs,trouble,mine,though,times,different,killed,hospital,anybody,Sam," +
                            "alright,wedding,shut,able,die,perfect,police,stand,comes,hit,story,ya,mm,waiting,dinner,against,funny,husband,almost,stupid,pay,answer,four,office,cool,eyes,news,child,shouldn't,half,side,yours,moment,sleep,read,where's," +
                            "started,young,men,sounds,sonny,lucky,pick,sometimes,'em,bed,also,date,line,plan,hours,lose,fire,free,hands,serious,Leo,shit,behind,inside,high,ahead,week,wonderful,T,fight,past,cut,quite,number,he'll,sick,S,it'll,game,eat," +
                            "nobody,goes,death,along,save,seems,finally,lives,worried,upset,Theresa,Carly,Ethan,met,book,brought,seem,sort,safe,living,children,weren't,leaving,front,shot,loved,asking,running,clear,figure,hot,felt,six,parents,drink," +
                            "absolutely,how's,daddy,sweet,alive,Paul,sense,meant,happens,David,special,bet,blood,ain't,kidding,lie,full,meeting,dear,coffee,seeing,sound,fault,water,fuck,ten,women,John,welcome,buy,months,hour,speak,lady,Jen,thinks," +
                            "Christmas,body,order,outside,hang,possible,worse,company,mistake,ooh,handle,spend,C,totally,giving,control,here's,marriage,realize,D,power,president,unless,sex,girls,send,needed,O,taken,died,scared,picture,talked,Jake,Al," +
                            "ass,hundred,changed,completely,explain,playing,certainly,sign,boys,relationship,Michael,loves,fucking,hair,lying,choice,anywhere,secret,future,weird,luck,she'll,Max,Luis,turned,known,touch,kiss,Crane,questions,obviously," +
                            "wonder,pain,calling,somewhere,throw,straight,Grace,cold,white,fast,Natalie,words,R,food,none,drive,feelings,they'll,worked,marry,light,test,drop,cannot,Frank,sent,city,dream,protect,twenty,class,Lucy,surprise,its,sweetheart," +
                            "forever,poor,looked,mad,except,gun,y'know,dance,takes,appreciate,especially,situation,besides,weeks,pull,himself,hasn't,act,worth,Sheridan,amazing,top,given,expect,Ben,rather,Julian,involved,swear,piece,busy,law,decided,black," +
                            "Joey,happening,movie,we'd,catch,Antonio,country,less,perhaps,step,fall,watching,kept,darling,dog,Ms,win,air,honor,personal,moving,till,admit,problems,murder,strong,he'd,evil,definitely,feels,information,honest,eye,broke,missed," +
                            "g,Ross,red,ent,trip,Brooke,E,club,Niles,suppose,calm,imagine,Todd,fair,caught,B,blame,street,sitting,favor,apartment,court,terrible,clean,Tony,learn,Alison,Rick,works,Rose,Frasier,relax,York,million,charity,accident,wake,prove," +
                            "Danny,smart,message,missing,forgot,small,interested,table,nbsp,become,Craig,mouth,pregnant,middle,Billy,ring,careful,shall,dude,team,ride,figured,wear,shoot,stick,Ray,follow,Bo,angry,instead,buddy,write,stopped,early,Angel,Nick," +
                            "ran,war,standing,forgive,jail,wearing,Miguel,ladies,kinda,lunch,Cristian,eight,Greenlee,gotten,hoping,Phoebe,thousand,ridge,music,Luke,paper,tough,tape,Emily,state,count,college,boyfriend,proud,agree,birthday,bill,seven,they've," +
                            "Timmy,history,share,offer,hurry,ow,feet,wondering,simple,decision,building,ones,finish,voice,herself,Chris,would've,list,Kay,mess,deserve,evidence,cute,Jerry,dress,Richard,interesting,Jesus,James,hotel,enjoy,Ryan,Lindsay,quiet," +
                            "concerned,road,Eve,staying,short,M,beat,sweetie,mention,clothes,finished,fell,neither,mmm,fix,Victor,respect,spent,prison,attention,holding,calls,near,surprised,bar,Beth,pass,keeping,gift,hadn't,putting,dark,self,owe,using,Nora," +
                            "ice,helping,bitch,normal,aunt,lawyer,apart,certain,plans,Jax,girlfriend,floor,whether,everything's,present,earth,private,Jessica,box,Dawson,cover,judge,upstairs,Alexis,Shawn,sake,mommy,possibly,worst,station,acting,accept,blow," +
                            "strange,saved,Ivy,conversation,plane,mama,yesterday,lied,quick,lately,stuck,lovely,security,report,Barbara,difference,rid,tv,Adam,store,she'd,bag,Mike,bought,ball,single,Kevin,doubt,listening,major,walking,cops,blue,deep,dangerous," +
                            "Buffy,park,sleeping,Chloe,Rafe,shh,record,lord,Erica,moved,join,key,captain,card,crime,gentlemen,willing,window,return,walked,guilty,Brenda,likes,fighting,difficult,soul,joke,service,magic,favorite,uncle,promised,public,bother," +
                            "island,Jim,seriously,cell,lead,knowing,broken,advice,somehow,paid,Blair,losing,push,helped,killing,usually,earlier,boss,Laura,beginning,liked,innocent,doc,rules,Elizabeth,Sabrina,summer,ex,cop,learned,thirty,risk,letting,Phillip," +
                            "speaking,officer,ridiculous,support,afternoon,Eric,born,dreams,apologize,seat,nervous,across,song,Olivia,charge,patient,Cassie,boat,how'd,brain,hide,detective,Aaron,Kendall,general,Tom,planning,nine,huge,breakfast,horrible" +
                            ",age,awful,pleasure,driving,hanging,picked,system,sell,quit,apparently,dying,notice,Josh,congratulations,chief,faith,Simon,gay,ho,one's,month,visit,Hal,could've,c'mon,aw,Edmund,Brady,letter,decide,American,double,Troy,sad,press," +
                            "forward,fool,showed,smell,seemed,Mary,spell,Courtney,memory,Mark,Alan,pictures,Paris,slow,Joe,Tim,seconds,hungry,board,position,hearing,Roz,kitchen,ma'am,Bob,force,fly,during,space,should've,realized,experience,kick,others,grab," +
                            "mother's,P,Sharon,discuss,third,cat,fifty,responsible,Jennifer,Philip,miles,fat,reading,idiot,yep,rock,rich,suddenly,agent,bunch,destroy,bucks,track,shoes,scene,peace,arms,demon,Diane,Bridget,Brad,low,Livvie,consider,papers,medical," +
                            "incredible,witch,er,drunk,attorney,Charlie,tells,knock,Karen,ways,eh,belle,cash,gives,department,nose,Skye,turns,keeps,beer,jealous,drug,Molly,sooner,cares,plenty,extra,tea,won,attack,ground,whose,outta,Kyle,L,weekend,matters,wrote," +
                            "type,father's,Alex,gosh,opportunity,king,impossible,books,machine,waste,th,pretend,named,danger,wall,Liz,Ian,Henry,jump,eating,proof,complete,slept,career,arrest,star,Phyllis,Mac,breathe,perfectly,warm,pulled,Maria,twice,easier," +
                            "killer,goin',dating,suit,romantic,drugs,comfortable,Isaac,powers,finds,checked,fit,divorce,begin,ourselves,closer,ruin,although,smile,laugh,fish,Abigail,treat,god's,fear,Anna,what'd,Simone,Amber,guy's,otherwise,excited,mail,hiding," +
                            "cost,green,stole,Pacey,noticed,Liza,fired,Daphne,Whitney,excellent,lived,bringing,pop,piper,bottom,note,sudden,church,bathroom,flight,Chad,la,honestly,sing,Katie,foot,games,glass,N,Mitch,remind,bank,Rory,charges,witness,finding," +
                            "places,tree,dare,hardly,that'll,U,interest,steal,princess,silly,contact,teach,shop,plus,colonel,fresh,trial,invited,roll,radio,art,reach,heh,dirty,choose,emergency,dropped,butt,credit,obvious,cry,locked,Larry,loving,positive,nuts," +
                            "agreed,Prue,price,goodbye,condition,guard,fuckin',grow,cake,mood,dad's,Bianca,total,crap,crying,Paige,K,belong,lay,partner,trick,pressure,ohh,arm,dressed,cup,lies,bus,taste,neck,south,something's,nurse,raise,land,cross,lots,mister," +
                            "carry,group,whoever,Eddie,drinking,they'd,breaking,file,lock,computer,yo,Rebecca,wine,closed,writing,spot,paying,study,assume,asleep,man's,turning,legal,justice,Viki,Chandler,bedroom,shower,Nikolas,camera,fill,reasons,forty,bigger," +
                            "nope,keys,Starr,breath,doctors,pants,freak,level,French,movies,gee,Monica,action,area,folks,Steve,cream,ugh,continue,focus,wild,truly,Jill,desk,convince,client,threw,Taylor,band,hurts,Charles,spending,Nel,field,allow,grand,answers," +
                            "shirt,chair,Christ,allowed,rough,doin,sees,government,Harry,ought,empty,round,lights,insane,hall,hat,bastard,wind,shows,aware,dealing,pack,meaning,flowers,tight,hurting,ship,subject,guest,mom's,chicken,pal,match,Elaine," +
                            "arrested,sun,Rachel,Salem,confused,surgery,expecting,deacon,Colleen,unfortunately,goddamn,lab,passed,bottle,beyond,whenever,pool,opinion,naked,held,common,starts,jerk,secrets,falling,played,necessary,barely,dancing,health,tests," +
                            "copy,Keri,video,cousin,planned,Vanessa,dry,ahem,twelve,simply,Tess,Scott,skin,often,English,fifteen,spirit,speech,names,issue,orders,nah,final,Michelle,America,St,results,code,Ned,Bonnie,W,believed,complicated,umm,research,nowhere," +
                            "escape,biggest,restaurant,page,grateful,usual,burn,Chicago,Austin,address,within,someplace,screw,everywhere,train,film,regret,goodness,mistakes,heaven,details,responsibility,suspect,corner,hero,dumb,terrific,Peter,mission,further," +
                            "Amy,gas,whoo,hole,memories,o'clock,Brian,truck,following,ended,nobody's,Margo,teeth,ruined,Hank,split,Reva,bear,airport,bite,smoke,Stenbeck,older,liar,horse,Gwen,showing,van,project,cards,desperate,themselves,search,pathetic,damage," +
                            "spoke,quickly,scare,Marah,G,beach,Mia,brown,afford,vote,settle,gold,re,mentioned,ed,due,passion,Y,stayed,rule,Friday,checking,tie,hired,upon,rush,Tad,heads,concern,blew,natural,Alcazar,Kramer,champagne,connection,tickets,Kate,finger," +
                            "happiness,form,saving,kissing,Martin,hated,personally,suggest,prepared,build,leg,onto,leaves,downstairs,ticket,it'd,taught,loose,holy,staff,sea,Asa,planet,duty,convinced,throwing,defense,Harvey,kissed,legs,Dave,according,loud,practice," +
                            "Andy,Jess,Saturday,Colin,bright,Amanda,Fraser,F,babies,army,where'd,warning,miracle,carrying,flying,Caleb,blind,queen,ugly,shopping,hates,someone's,Seth,monster,sight,vampire,Rosanna,bride,coat,account,states,clearly,celebrate,Nicole," +
                            "brilliant,wanting,Allison,add,moon,Forrester,lips,custody,center,screwed,buying,size,toast,thoughts,Isabella,student,stories,however,professional,stars,reality,Jimmy,birth,Lexie,attitude,advantage,grandfather,Sami,sold,opened,Lily," +
                            "grandma,beg,Edward,changes,Diego,Cole,someday,grade,cheese,roof,Kenny,Bobby,pizza,brothers,X,signed,bird,ahh,marrying,powerful,grown,grandmother,fake,opening,Sally,Stephanie,expected,eventually,must've,ideas,exciting,covered,Parker," +
                            "de,familiar,bomb,'bout,television,harmony,color,heavy,schedule,records,H,dollar,capable,master,numbers,Toby,practically,including,correct,clue,forgotten,immediately,appointment,social,nature,ú,deserves,west,Blake,teacher,threat," +
                            "Frankie,bloody,lonely,Kelly,ordered,shame,Brittany,local,jacket,hook,destroyed,scary,loser,investigation,above,Jamal,invite,shooting,merry,port,precious,lesson,Roy,criminal,growing,caused,victim,professor,followed,funeral,nothing's," +
                            "dean,considering,burning,couch,strength,harder,loss,view,Gia,beauty,sisters,everybody's,several,pushed,Nicholas,written,somebody's,shock,pushing,heat,chocolate,greatest,Holden,miserable,Corinthos,nightmare,energy,brings,Zander," +
                            "character,became,famous,enemy,crash,chances,sending,recognize,healthy,boring,feed,engaged,Sarah,percent,headed,Brandon,lines,treated,purpose,north,knife,rights,drag,San,fan,badly,speed,Santa,hire,curious,paint,pardon,Jackson,built," +
                            "behavior,closet,candy,Helena,warn,gorgeous,post,milk,survive,forced,daria,victoria,operation,suck,offered,hm,ends,dump,rent,marshall,remembered,lieutenant,trade,thanksgiving,rain,revenge,physical,available,program,prefer,baby's,spare," +
                            "pray,disappeared,aside,statement,sometime,animal,sugar,Ricky,meat,fantastic,breathing,laughing,itself,tip,stood,market,Raul,affair,Stephen,ours,depends,cook,babe,main,woods,protecting,jury,Harley,national,brave,storm,large,prince," +
                            "Jack's,interview,Daniel,roger,football,fingers,murdered,Stan,sexy,Julia,explanation,da,process,picking,based,style,stone,pieces,blah,assistant,stronger,block,aah,Newman,bullshit,pie,handsome,unbelievable,anytime,nearly,Maureen,shake," +
                            "everyone's,Oakdale,cars,wherever,serve,pulling,points,medicine,facts,waited,Pete,lousy,circumstances,stage,Lucas,disappointed,weak,trusted,license,nothin,community,trey,Jan,trash,understanding,slip,cab,Abby,sounded,awake,friendship," +
                            "stomach,weapon,threatened,Don,mystery,Sean,official,Lee,dick,regular,Donna,river,Malcolm,Vegas,valley,understood,contract,bud,sexual,race,basically,switch,lake,frankly,issues,cheap,lifetime,deny,painting,ear,clock,Baldwin,weight," +
                            "garbage,why'd,tear,ears,dig,bullet,selling,setting,indeed,gus,changing,singing,tiny,particular,draw,decent,Susan,super,spring,santos,avoid,messed,united,filled,touched,score,people's,disappear,stranger,exact,pills,kicked,harm,recently," +
                            "ma,snow,fortune,strike,pretending,raised,annie,slayer,monkey,insurance,fancy,Sydney,drove,cared,belongs,nights,shape,dogs,Lorelai,Jackie,base,Maggie,lift,Lewis,stock,Sonny's,fashion,freedom,timing,Johnny,guarantee,chest,bridge," +
                            "woke,Tabitha,source,patients,theory,lisa,camp,original,juice,burned,access,watched,heading,selfish,oil,drinks,wise,Morgan,Ashley,failed,period,doll,committed,elevator,freeze,noise,exist,science,pair,edge,wasting,sat," +
                            "player,ceremony,cartman,pig,uncomfortable,Ted,peg,guns,vacation,staring,files,bike,weather,name's,mostly,stress,Kristina,sucks,permission,arrived,thrown,possibility,faster,example,borrow,Casey,release,ate,notes,joy,hoo,library," +
                            "junior,property,negative,fabulous,event,doors,screaming,vision,Nancy,member,bone,battle,Xander,Giles,safety,term,devil,what're,meal,fellow,asshole,apology,anger,honeymoon,wet,bail,parking,fucked,non,hung,protection,manager,fixed," +
                            "families,dawn,sports,Chinese,campaign,map,wash,stolen,sensitive,stealing,photo,chose,Russell,lets,comfort,worrying,whom,pocket,Mateo,bleeding,students,shoulder,ignore,fourth,neighborhood,FBI,talent,Spaulding,Carmen,tied,garage," +
                            "dies,demons,travel,Diana,success,dumped,witches,training,rude,crack,model,bothering,radar,grew,willow,remain,soft,meantime,gimme,connected,chase,kinds,cast,cancer,Abe,v,sky,likely,Laurence,fate,buried,hug,brother's,driver,concentrate," +
                            "throat,prom,messages,east,unit,intend,Hayward,Dan,crew,ashamed,somethin,midnight,manage,guilt,weapons,terms,interrupt,guts,tongue,distance,conference,treatment,shoe,Kane,basement,Alexandra,sentence,purse,Hilda,glasses,cabin,universe," +
                            "towards,repeat,mirror,wound,Travers,Matthew,tall,reaction,odd,engagement,therapy,letters,emotional,runs,magazine,jeez,decisions,soup,daughter's,thrilled,Buchanan,society,managed,Dixie,sue,stake,rex,chef,moves,awesome,genius,extremely," +
                            "entirely,tory,nasty,moments,expensive,counting,shots,kidnapped,square,Seattle,son's,London,cleaning,shift,plate,Zack,impressed,smells,trapped,male,tour,Aidan,knocked,charming,attractive,argue,Sunday,puts,whip,language,heck,embarrassed," +
                            "settled,package,laid,animals,hitting,disease,bust,stairs,Lizzie,alarm,pure,nail,nerve,incredibly,hill,walks,lane,dirt,bond,stamp,sister's,becoming,terribly,friendly,easily,damned,jobs,suffering,disgusting,Washington,stopping,deliver," +
                            "riding,helps,federal,disaster,bars,DNA,crossed,rate,create,trap,claim,Christine,California,talks,eggs,effect,chick,turkey,threatening,spoken,snake,introduce,rescue,confession,embarrassing,bags,lover,impression,gate,Kim,fantasy,year's," +
                            "reputation,balls,attacked,among,lt,knowledge,presents,inn,Europe,chat,suffer,Bryant,argument,talkin,crowd,Montgomery,homework,fought,coincidence,cancel,accepted,rip,pride,solve,hopefully,Walter,pounds,pine,mate,illegal,generous,Tommy," +
                            "streets,Matt,director,glen,con,separate,outfit,maid,bath,punch,Phil,mayor,Helen,freaked,begging,recall,enjoying,bug,woman's,prepare,parts,wheel,signal,Nikki,direction,defend,signs,painful,Caroline,yourselves,walls,rat,Maris,amount," +
                            "that'd,suspicious,hearts,flat,cooking,button,warned,sixty,pity,parties,crisis,Rae,coach,Abbott,row,baseball,yelling,leads,awhile,pen,confidence,offering,falls,carter,image,farm,pleased,panic,Monday,hers,gettin,smith,role,refuse," +
                            "determined,Jane,hell's,grandpa,progress,Mexico,testify,passing,military,choices,artist,William,wh,uhh,gym,cruel,wings,traffic,pink,bodies,mental,gentleman,coma,poison,cutting,Proteus,guests,girl's,expert,bull,benefit,bell,faces,cases," +
                            "Mimi,ghost,led,jumped,Audrey,toilet,secretary,sneak,q,mix,Marty,Greta,firm,Halloween,Barry,agreement,privacy,dates,anniversary,smoking,reminds,pot,created,Wesley,twins,swing,successful,season,scream,considered,solid,options,flash," +
                            "commitment,senior,ill,else's,crush,ambulance,wallet,Thomas,Logan,discovered,officially,gang,til,rise,reached,eleven,option,laundry,former,assure,stays,skip,hunt,fail,accused,wide,Robert,challenge,Snyder,popular,learning,discussion," +
                            "clinic,plant,exchange,betrayed,bro,sticking,university,target,members,lower,bored,mansion,soda,silver,sheriff,suite,handled,busted,senator,Harold,load,happier,younger,studying,romance,procedure,ocean,section,Fred,winter,sec,commit," +
                            "bones,assignment,suicide,spread,Quinn,minds,fishing,swim,ending,bat,yell,llanview,league,chasing,seats,proper,holiday,command,believes,humor,hopes,fifth,winning,solution,leader,yellow,Theresa's,sharp,sale,Randy,lawyers,giant,nor," +
                            "material,latest,ash,highly,escaped,audience,winner,parent,burns,tricks,insist,dropping,cheer,medication,higher,flesh,district,wood,routine,Zelda,cookies,century,shared,sandwich,psycho,handed,false,beating,appear,adult,warrant,spike," +
                            "garden,family's,awfully,odds,article,treating,thin,suggesting,Palmer,fever,female,sweat,silent,specific,clever,sweater,request,prize,mall,tries,mile,manning,fully,estate,diamond,union,sharing,Jamie,assuming,judgment,goodnight,divorced," +
                            "quality,despite,Colby,surely,steps,jet,confess,Bart,mountain,math,listened,comin,answered,vulnerable,Boston,bless,dreaming,rooms,Claire,chip,zero,potential,pissed,Nate,kills,grant,wolf,tears,knees,chill,Carly's,blonde,brains,agency," +
                            "Harvard,degree,unusual,wife's,joint,rob,packed,Mel,dreamed,cure,covering,newspaper,lookin,coast,grave,egg,direct,cheating,breaks,quarter,orange,mixed,locker,husband's,gifts,brand,awkward,toy,Thursday,rare,policy,pilar,kid's,joking," +
                            "competition,classes,assumed,reasonable,dozen,curse,Quartermaine,millions,dessert,rolling,detail,alien,served,delicious,closing,vampires,released,Mackenzie,ancient,wore,value,tail,site,secure,salad,murderer,Margaret,hits,toward," +
                            "spit,screen,pilot,penny,offense,dust,conscience,Carl,bread,answering,admitted,lame,invitation,hidden,grief,smiling,path,homer,destiny,del,stands,bowl,pregnancy,Laurie,Hollywood,co,prisoner,delivery,Jenny,guards,desire"
                    ).toLowerCase().split(",")
        )
        for (i in list) r.add(
            Word(
                i,
                9000 + r.size,
                "null",
                "",
                ""
            )
        )
        return r.subList(0, if (level < 99) ((r.size / 100) * level) else r.size)
    }

    fun getFakeUsers(count: Int): List<User> {
        var r = ArrayList<User>()
        for (i in 0..count) r.add(
            User(
                name = names[i % 5],
                email = names[i % 5].toLowerCase()[0] + "." + names[i % 5].toLowerCase() + "@" + emails[i % 5],
                age = (i % 5) * 7 + 10,
                vocabulare = getGlobalDictionary((i % 5) * 20 + (i % 5) * 5),
                library = getFakeLibrary(),
                scores = (i % 5) * 20 + (i % 5) * 5,
                img = images[i % 5]
            )
        )
        return r
    }

    fun getFakeLibrary(): List<Book> {
        val inputfakeinfo = listOf(
            "A typical American. #*#What makes every American a typical one is a desire to get" +
                    " a well-paid job that will cover their credit card. A credit card is an indispensable" +
                    " part of life in America. In other words, any American knows that how he or she handles" +
                    " their credit card or cards, either will help them or haunt them for years.In the U.S. ge" +
                    "tting a card isn’t as hard as it used to be. Some companies now mail applications to high " +
                    "school students, rather than waiting for them to get into college. At their best credit cards" +
                    " allow their owners to reserve hotel rooms, rent cars and finance larger purchases over" +
                    " several months. At their worst, cards allow people with poor money management skills to" +
                    " get into a high-interest debt.For those who are deep in credit card debt, there are some" +
                    " Credit Services agencies that offer anyone in America both online or telephone, and face-" +
                    "to-face counseling. The counselors may propose a debt management plan. The plan includes lower" +
                    " interest payments for clients and setting up a pay off timetable. The agencies’ average client" +
                    " makes about \$32,000 a year and has more than \$16,000 debt in the program. Usually the " +
                    "agencies charge zero up to \$30 monthly to manage the debt service plan.Credit Services " +
                    "counselors advise, that if an American gets divorced, he or she may eliminate all joint " +
                    "debts by paying them off or transferring debt into a one-name account. Even in happy marr" +
                    "iages, the agency advises that the husband and wife should each have a credit card in their" +
                    " name only to establish separate credit histories.Once debts have been repaid, an American " +
                    "can re-establish hisher good credit by applying for a secured credit card and paying the " +
                    "balance off regularly. A store credit card usually is the next step, followed by applying " +
                    "for a major credit card such as a Visa or MasterCard.\n",
            "British food#*#I am always both amused and annoyed when I hear foreign people criicize" +
                    " British food. »It’s unimaginative,» they say. »It’s boring, it’s tasteless, and it’" +
                    "s chips with everything and totally overcooked vegetables.». I have a theory about British " +
                    "cooking, and I was interested to read that several famous cookery writers agree with me. My " +
                    "theory is this. Our basic ingredients, when fresh, are so full of flavor that we haven’t had " +
                    "to invent sauces and complex recipes to disguise their natural taste. What can compare with " +
                    "fresh peas or new potatoes just boiled (not over boiled) and served with butter? Why drown " +
                    "spring lamb a wine or cream or yoghurt and spices, when with just one or two herbs it is " +
                    "absolutely delicious? If you ask foreigners to name some typically English dishes, they will" +
                    " probably say »fish and chips» and then stop. It is disappointing, but true, that there is " +
                    "no tradition in Britain of eating in restaurants, because our food doesn’t lend itself to such " +
                    "preparation. British cooking is found in the home, where it is possible to time the dishes to " +
                    "perfection. So it is difficult to find a good English restaurant with reasonable prices. It is " +
                    "for these reasons that we haven’t exported our dishes, but we have imported a surprising number " +
                    "from all over the world. In most cities in Britain you’ll also find Indian, Chinese, French and " +
                    "Italian restaurants. In London you’ll also find Indonesian, Lebanese, Iranian, German, Spanish, " +
                    "Mexican, Greek… Cynics will say that this is because we have no »cuisine» ourselves, but, well, " +
                    "you know what I think!\n" ,
            "Animals in South America#*#South America is famous for its wild animals. There are man" +
                    "y kinds. We saw some of them. We drove to the jungle in the north of Brazil. We were really lucky. " +
                    "We walked very quietly and we saw a jaguar. It is a kind of South American cheetah. It was beautiful " +
                    "and very fast. I was really scared. I think Jaguars like eating hedgehogs!Then we went by boat up the " +
                    "Amazon river in the east of Brazil. It is the biggest river in South America. We saw alligators there. " +
                    "They were three metres long, fast and have big teeth! They are like crocodiles. We didn’t stay there " +
                    "long. We didn’t know. Maybe alligators like eating hedgehogs too!In Brazil we took a helicopter to " +
                    "the desert. There we met an unusual animal: an armadillo. Armadillos aren’t very big but they are " +
                    "very strong. They have got a ’house’ on their backs like a tortoise. Armadillos don’t eat hedgehogs" +
                    " — they eat insects!The last animal we saw in the forest in Brazil was a vampire bat. It was small and " +
                    "slow but … Heeeeelp! Do you know the story of Dracula? He met a vampire bat. It drank his blood. We didn" +
                    "’t stay long in that forest!\n" ,
            "Contemporary visual art in America #*#»I am always happier when I’m painting. It is " +
                    "like Christmas every day… waking up to see what I painted the day before». Rosanna Hardin Hill, artist. " +
                    "The works of Rosanna Hardin Hill are masterful depictions of ruins in Pompeii, lush gardens in Venice, " +
                    "villas in Florence, classical, romantic, historical scenes for sure. But there is another intriguing work " +
                    "that is not quite as obvious as oil paintings — the story behind the artist herself. How Hardin Hill " +
                    "became one of Indianapolis’ (Indiana) most recognized contemporary artists started with an assignment " +
                    "she received while working as a lifestyle reporter years ago. The assignment was to make a report on the " +
                    "lives of students studying at the Heron School of Art. »The reminded me of how artists live in Paris. It " +
                    "was a very bohemian lifestyle», recalls Hardin Hill. The dean of the school took notice of Hardin Hill’s " +
                    "skills at writing and her keen interest in the arts. He offered her a position in public relations with " +
                    "the bonus of taking classes free of charge. Hardin Hill, who was 30 at the time, took the bait. However, " +
                    "once she graduated with an arts degree, Hardin Hill still didn’t see the possibilities of making a living " +
                    "in the arts. So, she decided to take a job with a newspaper in Santa Fe, NM. Eventually, the lure of the " +
                    "arts was too much. Hardin Hill decided to »do just arts». She taught a beginning art class and sold her " +
                    "art works Hill supplemented her income with odd jobs, when she returned to New Mexico. Later, Hardin Hill " +
                    "transformed the cottage behind her home and studio into an art gallery. The property on Woodruff Place — " +
                    "the home and the cottage — has been in her family for three generations. Her grandparents first purchased " +
                    "it in 1915. The historic Eastside neighborhood has had an impact on her works. Hardin Hill has never " +
                    "regretted her decision to pursue the arts. »I love classical subjects and formal gardens», says Hardin " +
                    "Hill. »The influence of Woodruff Place really got me into formal gardens. It is like living in a park, " +
                    "with its fountains, statues and wonderful trees. Everything I think about — history, philosophy, and beauty " +
                    "in nature — it all just comes together in art for me».\n" ,
            "British TV#*#Television in Britain is a part of mass media, a single public structure." +
                    " It provides the society with up-to-date detailed information, which concerns political, economical, social, " +
                    "cultural and other important aspects. British people are fond of watching TV. Most families watch TV more than " +
                    "4 hours a day. Nowadays there is a big choice of channels and programmes. Everyone may choose something to their " +
                    "own taste. Broadcasting by television and radio in Britain is regulated by the Minister of Posts and " +
                    "Telecommunications. Television services are provided by the British Broadcasting Corporation (BBC), the Independent " +
                    "Television Authority (ITA) and other companies. The history of British TV started in 1936, when the BBC launched the " +
                    "world’s first public television service. The BBC studio productions come from the London Television Theatre, eight " +
                    "main London studios, and fully equipped regional studios in Manchester, Birmingham, Cardiff Glasgow, Bristol and Belfa" +
                    "st. In addition, eight small interview studios (used mainly for short insertions into the news) have been established " +
                    "in England, Scotland and Wales. The first regular independent television (ITV) service appeared in September 1" +
                    "955, by a programme transmission from the ITA London station. By 1958 it had already 7 stations in all parts of " +
                    "Great Britain. The ITV programmes are produced at modern studio centres in London, Manchester, Birmingham, Cardiff, " +
                    "Glasgow, Southampton and Newcastle. The main items broadcast by the ITV were entertainment programmes, plays and seria" +
                    "ls, sport and films. Both the BBC and the ITV services provide programmes of music, drama, light entertainment, and films. " +
                    "Most popular are programmes on the arts, children’s and family programmes, interviews with outstanding personal" +
                    "ities, investigations into matters of public interests, news " +
                    "reports covering international and national news. The point of discussion about TV in Britain is advertising. Ad" +
                    "vertising is excluded from the television programmes of the BBC. The ITV has advertising intervals in and between " +
                    "programmes, which is often criticized, as it often spoils the programmes. According to the Television Act advertising " +
                    "should have certain restrictions. The ITA has also agreed rules with the Postmaster General about certain classes of programm" +
                    "es, in which advertisements may not be inserted. The discussions about advertising continue, but British people " +
                    "are fond of their TV and receive a lot of fun watching it.\n" ,
            "Big cities of the United Kingdom#*#The biggest cities of the United Kingdom are also g" +
                    "reat industrial and cultural centres. The biggest city of the country is London. The population of Greater " +
                    "London is now over 8 million people. Lots of men and women crowd the city at day-time. They are engaged in th" +
                    "e vast international business of London which has made it like no other place in the world. Many of the present-" +
                    "day commercial, financial, and civic institutions of the city have their roots in the 16th century, and some go " +
                    "even deeper. London is also a cultural centre of the England. The British Museum, located here is the largest in " +
                    "the world. In the evening you may choose between more than 50 theatres of the city. Among most famous city attractions are also " +
                    "Buckingham Palace, the Houses of Parliament, St. Paul’s Cathedral, Westminster Abbey, the Tower of London and m" +
                    "any others. Other big and famous cities of the United Kingdom are Birmingham, Glasgow, Liverpool, Manchester, " +
                    "Edinburgh, Belfast and others. They are famous for its companies, unique character and history. Birmingham is " +
                    "long famous as an international business centre. It has developed into a modern and exciting city, which buildings " +
                    "and shops are second to none. Birmingham is at heart " +
                    "of Britain’s motorway system. Massive post-war development brought exciting new buildings, but the best of t" +
                    "he old ones have been preserved. The city’s museum and art gallery has some of the finest examples of European" +
                    " painting. Birmingnam’s ultra-modern library is one of the largest and best stocked in Europe and includes the " +
                    "Shakespeare Memorial Library with 40,000 books in 90 languages. The city possesses several interesting churches and " +
                    "two cathedrals. Edinburgh, the capital of Scotland, is also a great city of more than half a million inhabitants. The city is " +
                    "built of stone, not brick. The houses look hard and solid. Some people would call them grim, especially " +
                    "on a wet day, but when the sun shines beautifully in the city the city looks fine. Its many bookshops, tavern" +
                    "s, and clubs some of world-famous people visited at different times. Among them were Dr. Johnson and Robert Burns. " +
                    "One of the famous avenues in the world is the Prince’s Street in Edinburgh. It is the finest street and a shopping " +
                    "area of the city. Glasgow is the third largest city of Great Britain. You may feel its industrial energy everywhere in the " +
                    "city. The city extends along both banks of the river Clyde. With each phase of its development it has stretched, un" +
                    "til its outskirts now lie several miles from the city centre. It is, by far, the largest and most populous city in " +
                    "the whole of Scotland. Glasgow is known the world over for its ship-building. Glasgow-built locomotives run in every part of " +
                    "the world. Today Glasgow is of such a size that it extends far over both banks of the river Clyde and bridges are a" +
                    "s essential for the conduct of activities as are the people themselves. Within a distance of a mile there are 7 bridg" +
                    "es. They carry road and rail traffic in and out of the city. No other city of Scotland has or needs as many river crossings as Glasgow. " +
                    "Cardiff, the capital of Wales, lies near the mouth of the river Taff. In the days of our great-grandparents C" +
                    "ardiff was a tiny village. Today there are about a quarter of a million people living there. Cardiff is now the " +
                    "largest town in Wales and is noted for its coal. There is also a delightful park in the city which everyone tries " +
                    "to see. This is Cathay’s Park. Few towns in the world have such fine public buildings as Cardiff. The Law Courts, City Hall and University " +
                    "buildings in Cathay’s Park are worthy of any city in the world. The United Kingdom of Great Britain and Norther" +
                    "n Ireland have many other cities and towns, that attract thousands of people from all over the world either on business or private visit. \n" ,
            "America’s music culture#*#America’s music culture would be incomplete without blues " +
                    "music. Thought it was created in the early decades of the 20th century, blues music has had a huge influence " +
                    "on American popular music up to the present days. In fact, many key elements we hear in pop, soul, rhythm and blues, rock " +
                    "and roll, have their beginnings in blues music. It has never been the leader in music sales. Blues music has " +
                    "retained a significant presence not only in concerts and festivals throughout the United States, but in the daily life of every person " +
                    "on the planet, as well. One can hear the sound of the blues in unexpected places, from a television commercial to a new c" +
                    "ountry or western song.The best known blues musician today is B.B. King. His fame is well-deserved. Born in Indianola, M" +
                    "ississippi in 1925, he earned the nickname »B.B.» (»Blues boy») while playing on radio programs in Memphis, Tennessee. " +
                    "From the 1940s through the 1960s, he played mostly in clubs in the South that had only black audiences. In 1948, he had a h" +
                    "it record with »Three A.M. Blues» and toured steadily thereafter. His fame spread as he played at blues festivals, con" +
                    "cert halls, universities, and on television shows across the country. No other blues artist has worked harder, than " +
                    "B. B. King in his many years of playing over three hundred shows a year. By the late 1960s, B. B. had perfected his famo" +
                    "us guitar style of vibrating the fingers of his left hand as he played, and bending notes to achieve the blues notes, that " +
                    "are such an integral part of blue music. This singing guitar sound, coupled with his expressive tenor voice, brought King g" +
                    "reat success in 1969 with the recording of »The Thrill is Gone». The song broke through the limited sales of the blues " +
                    "market to achieve mainstream success and brought B.B. a Gr" +
                    "ammy award. B.B’s songs display a wide range of emotions, in addition to the sadness, so fundamental to blue music. He co" +
                    "mbines humor with a keen understanding of human nature in »Everybody Lies a Little Sometimes» and »How Blue Can You " +
                    "Get». King’s long and distinguished career includes many musical collaborations. Young rock musicians, in particular, " +
                    "appreciate his contributions to their genre. In 1988 B.B. played guitar and sang on the hit song »When Love Comes to Town» b" +
                    "y the Irish band U2. In 2001 he recorded an award-winning record with Eric Clapton called »Riding with the King»’. In a" +
                    " nutshell, King’s guitar work has had a strong influence on thousands of guitar soloists to this day. B.B. King remains the blues’ greatest ambassador. \n" ,
            "American favorite holidays: Christmas#*#Christmas Day is one of the most favorite Amer" +
                    "ican holidays It is celebrated on December 25. Christmas is a Christian holiday marking the birth of the Christ " +
                    "Child. Decorating houses and yards with lights, putting up Christmas trees, giving gifts, and sending greeting cards have " +
                    "become traditions even for many non-Christian. Americans »here also a tradition to place a decorated Christmas tree in the " +
                    "White House, the official home in Washington D.C. of the President of the USA. The tradition of placing a decorated tree in " +
                    "the White House began in 1889 on Christmas morning during the Presidency of Benjamin Harrison. The President’s grandchildren " +
                    "young Benjamin and Mary McKee, led the Harrison household into the second floor Oval Room to take a look at the first White House " +
                    "Christmas tree. It was lit with candles. Filled stockings hung from the mantel. Presents, candy and nuts were distributed to family " +
                    "and staff. President Harrison gave turkeys and gloves to his employees. He received a silver-dollar-shaped picture holder from his " +
                    "daughter, Mame Harrison McKee. What began as a family gathering has become a national tradition. Over the years, the White House " +
                    "Christmas tree has reflected both the times and the tastes of the First " +
                    "Family. First Lady Frances Cleveland created a »technology savvy» tree in 1895 when she hung electric lights o" +
                    "n the White House tree. First Lady Jacqueline Kennedy began the tradition of Christmas Tree themes when she decorated the 1961 " +
                    "Christmas tree in toy trimmings from the Nutcracker Suite ballet by Tchaikovsky. Today, the First Lady selects a theme and taps " +
                    "the talents of American artisans, who give life to the idea. Laura Bush chose »Home for the Holidays» for the 2001 theme, which " +
                    "features replicas of the family homes of the nation’s Presidents. The Presidents Christmas tree can be viewed by anyone either in " +
                    "the news programs, or on the official web site of the White House.\n" ,
            "British famous people: Winston Churchill#*#The Right Honourable Sir Winston Leonard Sp" +
                    "encer-Churchill, KG. ОМ. CH. FRS (November 30, 1874 — January 24, 1965) was a British politician, best known as Prime Minister of " +
                    "the United Kingdom during World War II. At various times an author, soldier, journalist, legislator and painter, Churchill is gener" +
                    "ally regarded as one of the most important leaders in British and world history. Winston Churchill was born at Blenheim Palace, near " +
                    "Woodstock in Oxfordshire. Winston’s father, Lord Randolph Churchill, was a politician. Winston’s mother, Lady Randolph Churchill of B" +
                    "rooklyn, New York, was a daughter of American millionaire Leonard Jerome. As the son of a prominent politician, it was unsurprising that " +
                    "Churchill was soon drawn into politics himself. He started speaking at a number of Conservative meetings in the 1890s. In the 1906 general " +
                    "election, Churchill won a seat in Manchester. He served as Under Secretary of State for the Colonies. Churchill soon became the most prominen" +
                    "t member of the Government. At the outbreak of the Second World War Churchill was appointed First Lord of the Admiralty. He was an early support" +
                    "er of the pan-Europeanism that led to the formation of the European Common market and later the European Union (" +
                    "for which one of the three main buildings of the European Parliament is named in his honour). Miscellany — In 1953 he was awarded two m" +
                    "ajor honours. He was knighted and became Sir Winston Churchill and he was awarded the Nobel Prize for Literature »for his mastery of histori" +
                    "cal and biographical description as well as for brilliant oratory in defending exalted human values. He was named Time Magazine »Man of the " +
                    "Halt-Century» in the early 1950s. In 1959 Churchill inherited the title of Father of the House. He became the MP with the longest continuous " +
                    "service — since 1924. Churchill College, a constituent college of the University of Cambridge, was founded in 1960 as the national and commonwe" +
                    "alth memorial to Winston Churchill. Churchill was voted as »The Greatest Briton» in 2002 »100 Greatest Britons» poll sponsored by the BBC " +
                    "and voted for by the public.\n" ,
            " About myself#*#Hello! I am Jane. I am 21. I am a student. I consider it to be a speci" +
                    "al time in my life. Student life is always full of fun, new impressions and knowledge. I like being a student. Each day I learn something new " +
                    "and communicate with my friends. We meet almost every day. Sometimes we stay at the university after classes to prepare our homework for the next" +
                    " day or just to talk about our student life. I like spending time with my friends. We often visit each other. I can talk with them for hours. They " +
                    "can help me and support me in any situation. I can say the same about my parents, with whom I live. My mother is a very wise woman. She understands " +
                    "me. We are not only close relatives, but also close friends. We have the same favourite colours — green and blue. These are the colours of nature " +
                    "and the sky. Our family hobby is travelling. We like seeing new places, meeting new people, exchanging our impressions. We often travel in summer " +
                    "and in winter. I adore Turkey, Egypt, and France. These countries have their own traditions, unique nature and culture. My other hobbies are music " +
                    "and theatre. I often visit the theatre. I sympathize with the characters on the stage. I try to understand them and, finally, I find it easier to " +
                    "solve my own problems watching the " +
                    "play. I understand my family and friends better. I am grateful to them for being so close to me, for their understand¬ing and support."
        )


        var r: ArrayList<Book> = ArrayList<Book>()
        for (i in inputfakeinfo) {
            val b=i.split("#*#")
            r.add( Book( title = b[0], text = b[1]) )
        }
        return r
    }


    fun getBasicExam(): Exam {
        val data = "99=EVEN=ДАЖЕ,ЕЩЁ НЕ,ТОГДА=ДАЖЕ\n" +
                "265=LEAST=ЛИСТ,НАИМЕНЬШИЙ,ТЕРЯТЬСЯ=НАИМЕНЬШИЙ\n" +
                "323=ISSUE=БОЛЕЗНЬ,ЭССЕ,ВЫПУСК=ВЫПУСК\n" +
                "329=REMAIN=ОСТАВАТЬСЯ,УХОДИТЬ,ПРИВЫКАТЬ=ОСТАВАТЬСЯ\n" +
                "491=CLAIM=КЛЯТВА,ТРЕБОВАНИЕ,РАЗРЕШЕНИЕ=ТРЕБОВАНИЕ\n" +
                "538=СOUNCIL= КАРАНДАШ,ВЫВОД,СОВЕТ=СОВЕТ\n" +
                "803=MARKET=РЫНОК,ЖУРНАЛ,ПАКЕТ=РЫНОК\n" +
                "994=SENTENCE=ВЫВОД,ПРЕДЛОЖЕНИЕ,ЧУВСТВО=ПРЕДЛОЖЕНИЕ\n" +
                "1058=REVIEW=ПРЕДСТАВЛЕНИЕ,ВООБРАЖЕНИЕ,ОБЗОР=ОБЗОР\n" +
                "1220=AWARD=НАГРАДА,ВЫСТУПЛЕНИЕ,ОЖЕРЕЛЬЕ=НАГРАДА\n" +
                "1335=ABSENT=АБСЕНТ,ОТСУТСТВУЮЩИЙ,ОПОЗДАВШИЙ=ОТСУТСТВУЮЩИЙ\n" +
                "1408=HILL=ЗДОРОВЬЕ,ВЕТЧИНА,ХОЛМ=ХОЛМ \n" +
                "1565=RARE=РЕДКИЙ,СЫРОЙ,ЖЕЛЕЗНЫЙ=РЕДКИЙ\n" +
                "1583=ENEMY=СОПЕРНИК,ВРАГ,ДРУЖИЩЕ=ВРАГ \n" +
                "1749=RESOLUTION=РЕЗЕРВАЦИЯ,РЕВОЛЮЦИЯ,РАЗРЕШЕНИЕ=РАЗРЕШЕНИЕ\n" +
                "1821=SOUP=СУП,ЛУК,МЫЛО=СУП\n" +
                "1933=BEER=МЕДВЕДЬ,ПИВО,КОРИЧНЕВЫЙ=ПИВО\n" +
                "1991=PURE=ПУШИСТЫЙ,НЕВЕСОМЫЙ,ЧИСТЫЙ=ЧИСТЫЙ\n" +
                "2226=PROTEIN=БЕЛОК,УСИЛИТЕЛЬ,ЯЙЦО=БЕЛОК\n" +
                "2328=SOUL=СТУЛ,ДУША,МОМЕНТ=ДУША\n" +
                "2389=HABIT=ХОББИТ,РАССТАНОВКА,ПРИВЫЧКА=ПРИВЫЧКА\n" +
                "2463=SHOVEL=ЛОПАТА,ДУШ,ШОУ=ЛОПАТА\n" +
                "2734=NIP=ПРИЛЕЧЬ,ЗАЖАТЬ,ХРЮКАТЬ=ЗАЖАТЬ\n" +
                "2911=PROOF=СПОР,ЗАМЕТКА,ДОКАЗАТЕЛЬСТВО=ДОКАЗАТЕЛЬСТВО\n" +
                "3393=UNCLE=ДЯДЯ,ДЕДУШКА,БАБУШКА=ДЯДЯ\n" +
                "3840=APOLOGIZE=СОЖАЛЕТЬ,ИЗВИНЯТЬСЯ,РАЗРЕШАТЬ=ИЗВИНЯТЬСЯ\n" +
                "4009=COPPER=ВЕРТОЛЁТ,ЧЕЛЮСТЬ,МЕДЬ=МЕДЬ\n" +
                "4576=THIGH=БЕДРО,ШТЫРЬ,СУСТАВ=БЕДРО\n" +
                "5255=SKULL=СКАЛЬП,ЧЕРЕП,СТЕКЛО=ЧЕРЕП\n" +
                "5402=SOMETIMES=НИКОГДА,ВСЕГДА,ИНОГДА=ИНОГДА\n" +
                "6112=GOAT=КОЗА,ТЕЛЁНОК,ИНДЮК=КОЗА\n" +
                "6728=NITROGEN=КИСЛОРОД,АЗОТ,ВОДОРОД=АЗОТ"
        var q = ArrayList<Question>()
        for (i in data.split("\n")) {
            i.split("=")
            q.add(
                Question(
                    title = i.split("=")[0],
                    text = i.split("=")[1],
                    imgUrl = "",
                    answers = i.split("=")[2].split(","),
                    rightAnswers = listOf(i.split("=")[3]),
                    lastAnswer = ""
                )
            )
        }
        var r = Exam(questions = q)
        return r
    }

}