# MINTPRESSO

As a core service, API Server comminicates with Graph database and provides number of APIs to users.

## 프로젝트 설명 - Project description 

*설명 적어야함*

## 작업 방식 - Contribution

이 레파지토리는 민트프레소 서비스의 API 서버의 코드를 담고 있습니다. 상업적으로 이용되는 이 소프트웨어는 누구나 개발에 참여 할 수 있으며 물론 그에 따른 보상과 계약이 따릅니다.

[Hall 채팅](https://hall.com/networks/23842)에서 모든 커밋터와 대화하세요.
[Github 그룹](http://github.com/mintpresso)에서 모든 코드를 보세요.

아래의 규칙을 따라 작업을 진행하세요.

### 이슈 만들기 - Create new issue

민트프레소는 아직 만들어지는 중입니다. 개선 할 부분, 버그등을 [Issues](https://github.com/mintpresso/api-server/issues)에 남겨주세요. 새로 만들어진 이슈는 커밋터들이 검토를 하거나 여러분에게 더 많은 의견을 물어볼 수 있습니다. 문제를 해결하는 자신의 소스코드 patch를 포함하거나 pull-request를 남기면 더 빨리 해결될 수 있습니다.

### 일감 받아오기 - Assign your task

각 레파지토리마다 Github Issue가 있습니다. 열려있는 이슈를 살펴보고 자신의 의견을 댓글로 남기세요. @eces @admire93을 언급하면 바로 달려와 여러분에게 피드백을 줄지도 모릅니다.

다른 커밋터가 해당 이슈를 작업하기 전에 미리 알려주세요!

### 레파지토리 복사하기 - Fork this repository

Fork를 통해 이 레파지토리를 여러분 개인 계정으로 복사(fork)하세요. 자유롭게 작업하면 됩니다.

### 코딩하기 - Coding

커밋 메시지(commit message)는 반드시 **Fix, Add, Support, Remove, Clean, Refactor, Apply**와 같이 대문자로 시작하는 동사원형을 쓰세요.

### 코드 리뷰받기 - Review your codes via Pull Request

코드를 완성하셨다구요? 개인 계정으로 복사(fork)하여 수정된 코드는 pull-request를 통해 원본(parent) 레파지토리인 이 곳으로 합쳐(merge)질 수 있습니다. 리뷰를 남길때는 어떤 문제를 해결했는지 알려주세요. 테스트 과정을 마친 후 여러분의 코드를 수락(accept)합니다.

**git 또는 Github에 익숙하지 않다면 아래의 글을 읽어주세요. 저 또한 읽으며 시작했습니다.**

[GitHub Help](https://help.github.com)
[GitHub Flow](http://scottchacon.com/2011/08/31/github-flow.html)
[GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown#references)

## 시작하기 - Getting started

[Play Framework](http://www.playframework.com) 2.1 이상 버전을 설치하고, 마스터 브랜치 압축파일을 [다운로드](https://github.com/mintpresso/api-server/archive/master.zip)합니다.

```bash
$ cd api-server
$ play
[info] Loading project definition from /Users/eces/api-server/project
[info] Set current project to api-server (in build file:/Users/eces/api-server/)
       _            _
 _ __ | | __ _ _  _| |
| '_ \| |/ _' | || |_|
|  __/|_|\____|\__ (_)
|_|            |__/

play! 2.1.1 (using Java 1.6.0_45 and Scala 2.10.0), http://www.playframework.org

> Type "help play" or "license" for more information.
> Type "exit" or use Ctrl+D to leave this console.

[api-server] $ 
```

이제 명령어를 통해 서버를 키거나 끄면 됩니다.

- `run 15000` 15000번 포트로 실행합니다.
- `~run 15000` 15000번 포트로 실행합니다. 파일이 바뀌면 자동으로 다시 컴파일합니다.
- `test` 테스트합니다.

서버에 올리려면 컴파일을 하고 [차이라떼](https://github.com/eces/chai-latte)를 통해 디플로이 하면됩니다.
```bash
$ play clean compile stage
...
[success] Total time: 7 s, completed Jun 15, 2013 12:20:43 AM

$ deploy 15000
...
[5/5] DONE!

$
```

- 로컬(localhost)에서는 `application.conf`를 이용하지만 서버에서는 production.conf를 이용합니다. 그러니 새로 추가된 설정 값을 @eces에게 알려주세요.
- 차이라떼에서는 `deploy.conf`에서 설정 값을 받아옵니다. `deploy.before`와 `deplot.after`를 따로 사용할 수 있는데 이는 서버 디플로이동안 NewRelic에게 서비스장애가 아니라고 말해주는 것이니 신경 쓸 필요없어요! [차이라떼](https://github.com/eces/chai-latte) 설명이 잘 쓰여있으니 참고바랍니다.

## 이슈 관리 - Issue tracker
https://github.com/mintpresso/api-server/issues

## 만든 이 - Contributors
https://github.com/mintpresso/api-server/contributors

## 저작권 - License

### MINTPRESSO API SERVER AND PANEL
Free Software Foundation's [GNU AGPL v3.0](http://www.gnu.org/licenses/agpl-3.0.html).

### MINTPRESSO Supported Drivers
[Apache License v2.0](http://www.apache.org/licenses/LICENSE-2.0)

### Third parties created drivers
Up to the license owner of each driver; licenses will vary.

## 언어 - Language

개발 언어는 한국어, 문서는 영어, 홍보와 서비스는 한국어와 영어

Language use in development is Korean, documentation is English and promotion/service is both Korean and English.
