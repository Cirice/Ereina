# Ereina

A service/system for expressing language rules on input documents (for Persian texts), inspired by [Duckling](https://github.com/facebook/duckling). 

## Features:

* Written in Haskell
* RESTful API (Integration is seamless)
* Parallel execution of requests

## Installation 

There are some methods for installing Ereina on your system.

* First method, we recommend Haskell build tool [stack](https://haskell-lang.org/get-started):

```sh
$ stack build
$ stack exec Ereina
```

**Note**: For running tests before installation, run:

```sh
$ stack test
```

* Second method, as a docker container (a RESTful server behind nginx):

To be written ...

## Usage and Methods

* version: this method returns a message regarding Ereina's version

```sh
curl -GET  http://localhost:2319/version 

$ {"version":"0.1.0.0","message":"Hi there, I am Ereina."}% 
```

* fixSpaces: given a document, removes redundant spaces from the document

```sh
curl -d '{"document":" باید چاره ای برای   این کار اندیشید    " }' -H "Content-Type: application/json" -X POST http://localhost:2319/fixSpaces

$ {"document":"باید چاره ای برای این کار اندیشید"}% 
```

* dropStopwords: given a document, removes stopwords (some frequent words, usually in conjunctive roles) from the document

```sh
curl -d '{"document":" وگو با خبرنگار خبرگزاری فارس در پردیس در خصوص اوضاع ترافیکی محورهای مواصلاتی هراز، فیروزکوه و امام رضا (ع) اظهار کرد: محور هراز در مسیر جنوب به شمال در برخی نقاط دارای ترافیک سنگین بوده و بقیه مسیر دارای ترافیک پرحجم است.

وی بیان کرد: ترافیک محور فیروزکوه در مسیر فیروزکوه ـ بومهن عادی و روان است اما در محدوده شهر دماوند پرحجم گزارش شده است.

رئیس پلیس راه شرق استان تهران با بیان اینکه تردد در محور قدیم جاجرود ـ تهران به صورت عادی در جریان است، تصریح کرد: محورهای امام رضا (ع) و حرم تا حرم نیز دارای بار ترافیکی عادی و روان هستند.

وی در پایان متذکر شد: بیشترین حجم ترافیک در محورهای شرق استان تهران در محدوده امامزاده هاشم تا پیست آبعلی در جریان است. " }' -H "Content-Type: application/json" -X POST http://localhost:2319/dropStopwords

$ {"document":"خبرنگار خبرگزاری فارس پردیس اوضاع ترافیکی محورهای مواصلاتی هراز، فیروزکوه امام رضا (ع) اظهار کرد: محور هراز مسیر جنوب شمال نقاط ترافیک سنگین بقیه مسیر ترافیک پرحجم است. کرد: ترافیک محور فیروزکوه مسیر فیروزکوه ـ بومهن عادی روان محدوده شهر دماوند پرحجم گزارش است. رئیس پلیس شرق استان تهران تردد محور قدیم جاجرود ـ تهران عادی است، تصریح کرد: محورهای امام رضا (ع) حرم حرم ترافیکی عادی روان هستند. پایان متذکر شد: بیشترین حجم ترافیک محورهای شرق استان تهران محدوده امامزاده هاشم پیست آبعلی است."}% 
```

* dropPunctuations: given a document, removes punctuations

```sh
curl -d '{"document":" سیاوش محبی در گفت<200c>وگو با خبرنگار خبرگزاری فارس در پردیس در خصوص اوضاع ترافیکی محورهای مواصلاتی هراز، فیروزکوه و امام رضا (ع) اظهار کرد: محور هراز در مسیر جنوب به شمال در برخی نقاط دارای ترافیک سنگین بوده و بقیه مسیر دارای ترافیک پرحجم است.

وی بیان کرد: ترافیک محور فیروزکوه در مسیر فیروزکوه ـ بومهن عادی و روان است اما در محدوده شهر دماوند پرحجم گزارش شده است.

رئیس پلیس راه شرق استان تهران با بیان اینکه تردد در محور قدیم جاجرود ـ تهران به صورت عادی در جریان است، تصریح کرد: محورهای امام رضا (ع) و حرم تا حرم نیز دارای بار ترافیکی عادی و روان هستند.

وی در پایان متذکر شد: بیشترین حجم ترافیک در محورهای شرق استان تهران در محدوده امامزاده هاشم تا پیست آبعلی در جریان است. " }' -H "Content-Type: application/json" -X POST http://localhost:2319/dropPunctuations

$ {"document":"  سیاوش محبی در گفتوگو با خبرنگار خبرگزاری فارس در پردیس در خصوص اوضاع ترافیکی محورهای مواصلاتی هراز فیروزکوه و امام رضا ع اظهار کرد محور هراز در مسیر جنوب به شمال در برخی نقاط دارای ترافیک سنگین بوده و بقیه مسیر دارای ترافیک پرحجم است\n\nوی بیان کرد ترافیک محور فیروزکوه در مسیر فیروزکوه ـ بومهن عادی و روان است اما در محدوده شهر دماوند پرحجم گزارش شده است\n\nرئیس پلیس راه شرق استان تهران با بیان اینکه تردد در محور قدیم جاجرود ـ تهران به صورت عادی در جریان است تصریح کرد محورهای امام رضا ع و حرم تا حرم نیز دارای بار ترافیکی عادی و روان هستند\n\nوی در پایان متذکر شد بیشترین حجم ترافیک در محورهای شرق استان تهران در محدوده امامزاده هاشم تا پیست آبعلی در جریان است  "}%  
```

* extractDiseases: extracts occurrences of disease names based on [Wikipedia](https://fa.wikipedia.org/wiki/%D9%81%D9%87%D8%B1%D8%B3%D8%AA_%D8%A8%DB%8C%D9%85%D8%A7%D8%B1%DB%8C%E2%80%8C%D9%87%D8%A7)

```sh
curl -d '{"document": "فصل پاييز را همگان به نام فصل هزار رنگ مي‌شناسند، اما همين فصل رنگارنگ به دليل كوتاهي روز و بلندي شب فصلي افسرده قلمداد مي‌شود و در عوض فصل بهار به عنوان فصل طراوت و شكوفايي در اذهان همگان نقش بسته است.

هر يك از روزهاي سال از اهميت و ويژگي‌هاي خاصي برخوردار است اما پديده‌اي به نام سندروم افسردگي فصلي موجب بروز اختلالاتي در برخي افراد مي‌شود.

افسـردگي فصـلي معمولا با نام اختلال عاطفي فصلي (SAD) شنـاخـته مي‌شود و نوعي افسردگي است كه هر سال در يك زمان معين در فرد بروز مي‌كند. معمولا از پاييز و زمسـتان شـروع شـده و در بـهار يـا اوايل تابستان پايان مي‌يابد. ايـن بـيماري خيلي جدي‌تر از سندروم كم اهميت «بـي حوصلگي زمستان» است.
 " }' -H "Content-Type: application/json" -X POST http://localhost:2319/extractDiseases

$ {"diseases":["اختلال عاطفی فصلی","افسردگی"]}%  
```

* extractLocations: extracts names associated with locations (cities, provinces and towns in Iran) based on [Wikipedia](https://fa.wikipedia.org/wiki/%D9%81%D9%87%D8%B1%D8%B3%D8%AA_%D8%B4%D9%87%D8%B1%D9%87%D8%A7%DB%8C_%D8%A7%DB%8C%D8%B1%D8%A7%D9%86)

```sh
curl -d '{"document": "شهر کوچک، زیبا و کوهستانی سی سخت در ۳۵ کیلومتری شمال غربی یاسوج قرار دارد. هر دوی این شهرها با دارا بودن جاذبه‌های طبیعی بسیار از جمله دریاچه‌ها، رودخانه‌ها، چشمه‌ها، مناطق کوهستانی، تاکستان‌ها، دشت‌های پرگل، پارک‌ها و چندین روستای زیبا، همه ساله گردشگران مشتاق زیادی را از سراسر کشور و به خصوص مناطق جنوبی به سمت خود جذب می‌کنند. در ادامه با کجارو همراه باشید تا هر آنچه را که برای سفر به این دو شهر نیاز داریم با هم مرور کنیم.
" }'  -H "Content-Type: application/json" -X POST http://localhost:2319/dropStopwords

$ {"locations":["یاسوج","سی سخت"]}% 
```

* extractBrands: this method extracts  occurrences of more than 100 popular brands (mainly Iranian brands) from a Persian document

```sh
curl -d '{"document":" گروه صنعتی گلرنگ در سال ۱۳۸۲به ریاست هیئت‌مدیره استاد حاج محمد کريم فضلی و مدیرعاملی دکتر مهدی فضلی تأسیس شد. ریشه شکل‌گیری این گروه به سال ۱۳۵۱و تأسیس شرکت تولیدی ـ شیمیایی پاکشو توسط استاد حاج محمد کریم فضلی بر می‌گردد. محصولات اولیه شرکت پاکشو با برند گلرنگ روانه بازار شدند ولی به تدریج محصولات متنوع با برندهای مختلف به سبد محصولات گروه اضافه شد. گروه صنعتی گلرنگ با تکيه بر سرمایه انسانی باانگيزه و نوآور ارايه محصولات و خدمات با کيفيت و ارزش برتر، بهبود زندگی مشتريان و مصرف‌کنندگان... " }' -H "Content-Type: application/json" -X POST http://localhost:2319/extractBrands 
 
$ {"brands":["گلرنگ"]}%  
```

## Todos

* Adding more tests
* Adding more methods
* Writing more documentation

