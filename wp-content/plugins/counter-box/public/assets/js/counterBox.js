/*! ========= INFORMATION ============================
    - document:  Counter Box Pro
	- brand:     Wow-Company
	- brand-url: https://wow-company.com/
    - store-url: https://wow-estore.com/
	- author:    Dmytro Lobov
	- url:       https://wow-estore.com/item/counter-box-pro/
==================================================== */

'use strict';

const counterBox = function (selector, options, element) {

    // Default settings
    let _default = {
        container_css: ``, // Style for container
        number_css: ``, // Style for numbers
        type: 'CountToDate', // CountToDate, ContFromDate, CountToWeekday, Timer, UserTimer, Counter
        date_options: {
            date: '2025-05-15', // Date like 2020-05-15, Can be: Everyday, Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday
            time: '23:59:59', // Time: hours:minutes:seconds
            timezone: '+00:00', // time zone designator (+hh:mm or -hh:mm)
        },
        timer_options: {
            day: '0',
            hours: '0',
            minutes: '100',
            seconds: '10',
        },
        counter_options: {
            start: 1, // initial number for counter
            finish: 5, // initial number for counter
            speed: {min: 1, max: 1}, // min & max speed for counter in seconds
            increment: {min: 1, max: 1}, // min & max increment of number for the counter
            round: 2, // rounding the number. set decimals
            delimiter: 1, // delimiter of a numbers
            remember: 0, // remember number for user
            number: 0, // for counter with date
            variable: 1, // for counter with date

        },
        targets: {},
        titles: {},

        // Action
        active_url: {
            enable: false,
            url: 'counter=active'
        },
        referrer_url: {
            enable: false,
            url: ''
        },

        // GeoTargeting
        geotargeting: false,
        countries: [],
    };

    let settings = _objAssign(_default, options);

    // Helpers
    function _objAssign(target, source) {
        let objs = [target, source];
        return objs.reduce(function (r, o) {
            Object.keys(o).forEach(function (k) {
                r[k] = o[k];
            });
            return r;
        }, {});
    }

    function _count(obj) {
        return Object.keys(obj).length;
    }

    function _getDate(options) {
        options = options || false;
        if (options === false) {
            options = settings.date_options;
        }
        return new Date(options.date + 'T' + options.time + options.timezone);
    }

    function _weekdayNumder(arr, val) {
        for (let i = 0; i < arr.length; i++) {
            if (arr[i] === val) {
                return i;
            }
        }
        return false;
    }

    function _getWeekdayDate() {
        let new_date = settings.date_options;
        let daily = new_date.date;
        let date = new Date();
        let current_weekday = date.getDay();
        let weekday = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
        let weekday_num = _weekdayNumder(weekday, daily);
        if (daily !== 'Everyday' && weekday_num !== current_weekday) {
            date.setDate(date.getDate() + ((weekday_num + 7 - date.getDay()) % 7));
        }
        let year = date.getFullYear();
        let month = ('0' + (date.getMonth() + 1)).slice(-2);
        let day = ('0' + date.getDate()).slice(-2);
        new_date.date = year + '-' + month + '-' + day;
        return _getDate(new_date);
    }

    function _getTimerDate() {
        let options = settings.timer_options;
        let current = settings.type === 'TimerStopGo' ? 0 : new Date().getTime();
        let days = parseInt(options.day) * 86400000;
        let hours = parseInt(options.hours) * 3600000;
        let minutes = parseInt(options.minutes) * 60000;
        let seconds = parseInt(options.seconds) * 1000;
        let left = current + days + hours + minutes + seconds;
        if (settings.type === 'TimerStopGo') {
            left = left / 1000;
        }
        return left;
    }

    function _getUserTimerDate() {
        let date = localStorage.getItem(selector);
        if (date === null) {
            date = _getTimerDate();
            localStorage.setItem(selector, date.toString());
        }
        return parseInt(date);
    }

    function _updateTimer(left) {
        localStorage.setItem(selector, left.toString());
    }

    function _getCounterNumber() {
        let options = settings.counter_options;
        let userNumber = localStorage.getItem(selector);
        let number;
        if (parseInt(options.remember) === 1 && userNumber !== null) {
            number = userNumber;
        } else {
            number = parseFloat(options.start);
        }
        return parseFloat(number);
    }

    function _decimalNumber(value) {
        if (value.toString().length < 2) {
            return ('0' + value);
        }
        return value;
    }

    function _setResult(selector, value) {
        if (value < 0) value = 0;
        if (selector !== '.counter-box__year') {
            value = _decimalNumber(value).toString();
        }
        if (selector !== '.counter-box__day') {
            value = _decimalNumber(value).toString();
        }
        if (element.querySelector(selector)) {
            element.querySelector(selector).innerHTML = value;
        }
    }

    function _goCount(left) {
        const yearBox = element.querySelector('.counter-box__year');
        if(yearBox) {
            let years = Math.floor(left / 31557600);
            _setResult('.counter-box__year', years);
            left -= years * 31557600;
        }

        const daysBox = element.querySelector('.counter-box__day');
        if(daysBox) {
            let days = Math.floor(left / 86400);
            _setResult('.counter-box__day', days);
            left -= days * 86400;
        }

        const hoursBox = element.querySelector('.counter-box__hour');
        if(hoursBox) {
            let hours = Math.floor(left / 3600);
            _setResult('.counter-box__hour', hours);
            left -= hours * 3600;
        }

        const minBox = element.querySelector('.counter-box__min');
        if(minBox) {
            let minutes = Math.floor(left / 60);
            _setResult('.counter-box__min', minutes);
            left -= minutes * 60;
        }

        let seconds = left;
        _setResult('.counter-box__sec', seconds);
    }

    function _goTarget() {
        let targets = settings.targets;

        if (_count(targets) <= 0) return;

        for (let key in targets) {
            switch (key) {
                case 'hideBlock':
                    document.querySelector(targets[key]).style.display = 'none';
                    break;
                case 'showBlock':
                    document.querySelector(targets[key]).style.display = 'block';
                    break;
                case 'redirect':
                    window.location.replace(targets[key]);
                    break;
                case 'hideCounter':
                    if (targets[key] === '1') element.style.display = 'none';
                    break;
                case 'showMessage':
                    element.innerHTML = targets[key];
                    break;
                case 'action':
                    let fn = window[targets[key]];
                    if (typeof fn === 'function') {
                        fn();
                    }
                    break;
            }
        }
    }

    function goCountDown(date) {
        let left;
        let current_date = new Date();
        if (current_date > date) {
            left = 0;
            _goCount(left);
            _goTarget();
            return;
        }
        left = Math.floor((date - current_date) / 1000);
        _goCount(left);
        setTimeout(goCountDown, 1000, date);
    }

    function goTimer(left) {
        if (left < 0) {
            left = 0;
            _goCount(left);
            _goTarget();
            return;
        }
        _goCount(left);
        left = left - 1;
        _updateTimer(left);
        setTimeout(goTimer, 1000, left);
    }

    function goCountUp(date) {
        let current_date = new Date();
        let left;
        left = Math.floor((current_date - date) / 1000);
        _goCount(left);
        setTimeout(goCountUp, 1000, date);
    }

    function _randomInteger(min, max) {
        return parseFloat(min) + Math.random() * (parseFloat(max) - parseFloat(min));
    }

    function _delimiterNumber(number) {
        let locale = window.navigator.language;
        return new Intl.NumberFormat(locale).format(number);
    }

    function _numberOutput(number) {
        let options = settings.counter_options;
        number = parseFloat(number);

        let start_number = parseFloat(options.start);
        let start_finish = parseFloat(options.finish);

        let direct = start_number < start_finish ? 'up' : 'down';

        let stop = 'no';

        if (direct === 'up' && number > start_finish) {
            number = start_finish;
            stop = 'yes';
        } else if (direct === 'down' && number < start_finish) {
            number = start_finish;
            stop = 'yes';
        }

        let output = number.toFixed(parseInt(options.round));

        if (parseInt(options.delimiter) === 1) {
            output = _delimiterNumber(output);
        }

        if (parseInt(options.remember) === 1) {
            localStorage.setItem(selector, number);
        }

        let counters = document.querySelectorAll(`${selector} .counter-box__counter`);
        if (counters.length > 0) {
            counters.forEach((counter) => {
                counter.innerHTML = output;
            });
        }

        if (stop === 'yes') {
            _goTarget();
            return false;
        }
        return true;
    }

    function goCounter(number, check) {
        let options = settings.counter_options;

        let speed = _randomInteger(options.speed.min, options.speed.max);
        let amount = _randomInteger(options.increment.min, options.increment.max);
        if (check !== null) {
            number = number + amount;
        } else {
            check = 1;
        }

        if (_numberOutput(number) === true) {
            setTimeout(goCounter, speed * 1000, number, check);
        }
    }

    function _getCounterFromDateNumber() {
        let options = settings.counter_options;
        let dateOptions = settings.date_options;

        let currentDate = new Date();
        let targetDate = new Date(`${dateOptions.date}T${dateOptions.time}`);
        let diffInSeconds = Math.floor((currentDate - targetDate) / 1000);
        let base = parseFloat(options.number);
        let variable = parseFloat(options.variable);
        let number = base + (diffInSeconds * variable);
        return parseFloat(number);
    }

    function _getCounterToDateNumber() {
        let options = settings.counter_options;
        let dateOptions = settings.date_options;

        let currentDate = new Date();
        let targetDate = new Date(`${dateOptions.date}T${dateOptions.time}`);
        let diffInSeconds = Math.floor((targetDate - currentDate) / 1000);
        let base = parseFloat(options.number);
        let variable = parseFloat(options.variable);
        let number = parseFloat(base) - (diffInSeconds * variable);
        return parseFloat(number);
    }


    function _getCounterFromWeekdayNumber() {
        let options = settings.counter_options;
        let weekdays = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
        let dateOptions = settings.date_options;
        let dayOpt = dateOptions.date;
        let dayNum = _weekdayNumder(weekdays, dayOpt);

        let date = new Date();
        let year = date.getFullYear();
        let month = (date.getMonth() + 1).toString().padStart(2, '0');
        let day = date.getDate().toString().padStart(2, '0');
        let weekday = date.getDay();

        let currentDayTarget = new Date(`${year}-${month}-${day}T${dateOptions.time}`);

        if(dayNum !== false) {
            let diff = weekday - dayNum;
            if (diff < 0) {
                diff += 7;
            }
            currentDayTarget.setDate(currentDayTarget.getDate() - diff);
        }

        console.log(currentDayTarget)

        let diffInSeconds = Math.floor((date - currentDayTarget) / 1000);
        let base = parseFloat(options.number);
        let variable = parseFloat(options.variable);
        let number = base + (diffInSeconds * variable);
        return parseFloat(number);
    }

    function goCounterFromWeekday(number, check) {
        let options = settings.counter_options;

        let speed = _randomInteger(options.speed.min, options.speed.max);
        let amount = parseFloat(options.variable);

        if (check !== null) {
            number = number + speed * amount;
        } else {
            check = 1;
        }

        if (_numberOutput(number) === true) {
            setTimeout(goCounterFromWeekday, speed * 1000, number, check);
        }
    }

    function _getCounterToWeekdayNumber() {
        let options = settings.counter_options;
        let weekdays = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
        let dateOptions = settings.date_options;
        let dayOpt = dateOptions.date;
        let dayNum = _weekdayNumder(weekdays, dayOpt);

        let date = new Date();
        let year = date.getFullYear();
        let month = (date.getMonth() + 1).toString().padStart(2, '0');
        let day = date.getDate().toString().padStart(2, '0');
        let weekday = date.getDay();

        let currentDayTarget = new Date(`${year}-${month}-${day}T${dateOptions.time}${dateOptions.timezone}`);

        if(dayNum !== false) {
            let diff = dayNum - weekday ;
            if (diff < 0) {
                diff += 7;
            }
            currentDayTarget.setDate(currentDayTarget.getDate() + diff);
        }

        let diffInSeconds = Math.floor((currentDayTarget - date) / 1000);
        let base = parseFloat(options.number);
        let variable = parseFloat(options.variable);
        let number = parseFloat(base) - (diffInSeconds * variable);
        return parseFloat(number);


    }

    function goCounterToWeekday (number, check) {
        let options = settings.counter_options;

        let speed = _randomInteger(options.speed.min, options.speed.max);
        let amount = parseFloat(options.variable);

        if (check !== null) {
            number = number - speed * amount;
        } else {
            check = 1;
        }

        if (_numberOutput(number) === true) {
            setTimeout(goCounterToWeekday, speed * 1000, number, check);
        }
    }

    function goCounterToDate(number, check) {

        let options = settings.counter_options;

        let speed = _randomInteger(options.speed.min, options.speed.max);
        let amount = parseFloat(options.variable);

        if (check !== null) {
            number = number - speed * amount;
        } else {
            check = 1;
        }

        if (_numberOutput(number) === true) {
            setTimeout(goCounterFromDate, speed * 1000, number, check);
        }
    }

    function goCounterFromDate(number, check) {

        let options = settings.counter_options;

        let speed = _randomInteger(options.speed.min, options.speed.max);
        let amount = parseFloat(options.variable);

        if (check !== null) {
            number = number + speed * amount;
        } else {
            check = 1;
        }

        if (_numberOutput(number) === true) {
            setTimeout(goCounterFromDate, speed * 1000, number, check);
        }
    }


    // Replace tags in content
    function setElements() {
        let content = element.innerHTML;
        content = content.replace('{year}', '<span class="counter-box__year"></span>');
        content = content.replace('{day}', '<span class="counter-box__day"></span>');
        content = content.replace('{hour}', '<span class="counter-box__hour"></span>');
        content = content.replace('{min}', '<span class="counter-box__min"></span>');
        content = content.replace('{sec}', '<span class="counter-box__sec"></span>');
        content = content.replace('{counter}', '<span class="counter-box__counter"></span>');
        element.innerHTML = content;
    }

    function _forEach(items, callback) {
        // loops through elements ;of an array
        for (let i = 0; i < items.length; i++) {
            callback && callback(items[i], i);
        }
    }

    // set style for element
    function setStyle() {
        element.style.cssText = settings.container_css;
        let numbers = element.querySelectorAll('[class *= "counter-box__"]');
        _forEach(numbers, function (number) {
            number.style.cssText = settings.number_css;
        });
        if (_count(settings.titles) > 0) {
            let titles = settings.titles;
            let css = '';
            for (let prop in titles) {
                if (prop === 'css') {
                    css += `${selector} [class ^= "counter-box__"]:after { ${titles[prop]} }`;
                } else {
                    css += `${selector} .counter-box__${prop}:after{content: '${titles[prop]}';}`;
                }
            }
            if (css !== '') {
                let style = document.createElement('style');
                style.innerText = css;
                document.body.append(style);
            }
        }
    }

    function startCount() {
        let type = settings.type;
        let date, number;
        switch (type) {
            case 'CountToDate':
                date = _getDate();
                goCountDown(date);
                break;
            case 'ContFromDate':
                date = _getDate();
                goCountUp(date);
                break;
            case 'CountToWeekday':
                date = _getWeekdayDate();
                goCountDown(date);
                break;
            case 'Timer':
                date = _getTimerDate();
                goCountDown(date);
                break;
            case 'UserTimer':
                date = _getUserTimerDate();
                goCountDown(date);
                break;
            case 'TimerStopGo':
                date = _getUserTimerDate();
                goTimer(date);
                break;
            case 'Counter':
                number = _getCounterNumber();
                goCounter(number, null);
                break;
            case 'CounterFromDate':
                number = _getCounterFromDateNumber();
                goCounterFromDate(number, null);
                break;
            case 'CounterToDate':
                number = _getCounterToDateNumber();
                goCounterToDate(number, null);
                break;
            case 'CounterFromWeekday':
                number = _getCounterFromWeekdayNumber();
                goCounterFromWeekday(number, null);
                break;
            case 'CounterToWeekday':
                number = _getCounterToWeekdayNumber();
                goCounterToWeekday(number, null);
                break;
        }
    }

    function activateCounterUrl() {
        if (settings.active_url.enable !== true) {
            return true;
        }
        const counterParam = (settings.active_url.url).split('=');
        const paramName = counterParam[0];
        const paramVal = counterParam[1];
        const params = new URLSearchParams(document.location.search);
        const name = params.get(paramName);

        if (name === paramVal) {
            return true;
        }

        return false;
    }

    function referrerCounter() {
        if (settings.referrer_url.enable !== true) {
            return true;
        }

        if (settings.referrer_url.url === '') {
            return true;
        }
        const referrerUrl = document.referrer;

        return referrerUrl.includes(settings.referrer_url.url);
    }

    // Get current counter by Geojs.io
    function geoTargeting() {
        return new Promise((resolve, reject) => {
            fetch('https://get.geojs.io/v1/ip/country.json')
                .then(response => response.json())
                .then(data => {
                    // Assuming you're checking if country data is present
                    // Adjust condition based on your needs
                    if (settings.countries.includes(data.country)) {
                        resolve(true);
                    } else {
                        resolve(false);
                    }
                })
                .catch(error => {
                    console.error('Error:', error);
                    resolve(false);
                });

        });
    }


    async function counterRun() {

        if (settings.geotargeting !== false) {
            const geoResult = await geoTargeting();
            if (geoResult === false) {
                return;
            }
        }

        if (activateCounterUrl() !== true) {
            return;
        }
        if (referrerCounter() !== true) {
            return;
        }
        setElements();
        setStyle();
        startCount();
    }

    return counterRun();
};

document.addEventListener('DOMContentLoaded', function () {
    for (let key in window) {
        if (key.indexOf('CounterBox_') >= 0) {
            let val = window[key];
            let elements = document.querySelectorAll(val.selector);
            elements.forEach((element) => {
                new counterBox(val.selector, val, element);
            });
        }
    }
});