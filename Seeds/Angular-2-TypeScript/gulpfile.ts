import {Gulpclass, Task, SequenceTask} from 'gulpclass/Decorators';

/* Configuration. */
const BASEDIR = './';
const APPDIR = './app/';

const FILES = {
    TYPESCRIPTS: APPDIR + '**/*.ts',
    JAVASCRIPTS: APPDIR + '**/*.js',
    SOURCEMAPS: APPDIR + '**/*.js.map',
    TSCONFIG: 'tsconfig.json',
    KARMACONF: 'karma.conf.js'
};

/* Global require should always be available at runtime. */
declare function require(name: string): any;

/* Gulp modules and stuff. */
const gulp = require('gulp');
const del = require('del');
const tsc = require('gulp-typescript');
const sourcemaps = require('gulp-sourcemaps');
const Server = require('karma').Server;
const tslint = require('gulp-tslint');

@Gulpclass()
export class Gulpfile {

    private tsProject = tsc.createProject(FILES.TSCONFIG);

    /**
     * COMPILE.
     * Builds TypeScript source files into the application directory.
     */
    @Task('compile', ['lint'])
    compile() {
        const tsResult = this.tsProject.src()
            .pipe(sourcemaps.init())
            .pipe(tsc(this.tsProject));

        return tsResult.js
            .pipe(sourcemaps.write(BASEDIR))
            .pipe(gulp.dest((file) => file.base));
    }

    /**
     * LINT.
     * Runs tslint on all TypeScript source files in the application directory.
     */
    @Task()
    lint() {
        return gulp.src(FILES.TYPESCRIPTS)
            .pipe(tslint())
            .pipe(tslint.report('prose', {
                summarizeFailureOutput: true,
                emitError: false
            }));
    }

    /**
     * WATCH.
     * Watches TypeScript files in the application directory and compiles them
     * whenever they change.
     */
    @Task()
    watch(done: Function) {
        gulp.watch(FILES.TYPESCRIPTS, ['compile']);
        done();
    }

    /**
     * CLEAN.
     * Cleans compiled files from the application directory.
     */
    @Task()
    clean() {
        return del([
            FILES.JAVASCRIPTS,
            FILES.SOURCEMAPS]);
    }

    /**
     * TDD.
     * Runs the jasmine test suite in the karma runner, with file watches
     * and repeated runs on file changes.
     */
    @Task('tdd', ['compile'])
    tdd(done: Function) {
        new Server({
            configFile: __dirname + '/' + FILES.KARMACONF,
            autoWatch: true,
            singleRun: false
        }, done).start();
    }

    /**
     * TEST.
     * Runs the jasmine test suite in the karma runner, but only once.
     */
    @Task('test', ['compile'])
    test(done: Function) {
        new Server({
            configFile: __dirname + '/' + FILES.KARMACONF,
            autoWatch: false,
            singleRun: true
        }, done).start();
    }

    /**
     * DEFAULT.
     * Runs TDD.
     */
    @SequenceTask()
    default() {
        return ['clean', 'watch', 'tdd'];
    }

}
