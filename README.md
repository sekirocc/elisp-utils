# Emacs Lisp 实用脚本集合

这是一个收集了多个实用 Emacs Lisp 脚本的仓库，旨在增强 Emacs 的使用体验。

## 📦 包含的脚本

### 🎨 主题

#### bogster-theme.el
自定义的 Emacs 主题，提供舒适的编辑体验。

### 🪟 窗口管理

#### enhanced-winner-mode.el
增强的窗口配置管理工具，基于 winner-mode 进行了功能扩展，让窗口配置的保存和恢复更加智能。

#### init-popper.el
Popper 窗口的保存和恢复功能，用于管理临时弹出窗口（如帮助、编译信息等），提供更好的窗口组织体验。

### ✨ 高亮功能

#### my-highlight-current-line.el
当前行高亮显示，让你更容易追踪光标所在位置。

#### my-isearch-highlight-all.el
搜索时高亮所有匹配项，增强搜索的可视化效果。

### 🛠️ 实用工具

#### gitignore-generator.el
快速生成 .gitignore 文件的工具，支持多种编程语言和框架的模板。

#### init-wayland.el
在 Wayland 环境下的复制粘贴支持，解决 Emacs 在 Wayland 显示协议下与系统剪贴板的交互问题。

## 🚀 安装方法

### 单个脚本安装

将需要的脚本文件下载到你的 Emacs 配置目录（通常是 `~/.emacs.d/` 或 `~/.config/emacs/`）：

```bash
# 下载单个脚本
wget https://raw.githubusercontent.com/sekirocc/elisp-utils/main/脚本名.el -P ~/.emacs.d/lisp/

# 或使用 curl
curl -o ~/.emacs.d/lisp/脚本名.el https://raw.githubusercontent.com/sekirocc/elisp-utils/main/脚本名.el
```

然后在你的 `init.el` 或 `.emacs` 中加载：

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require '脚本名)
```



## 📝 使用说明

### enhanced-winner-mode
```elisp
(require 'enhanced-winner-mode)
(enhanced-winner-mode 1)
```

### init-popper
```elisp
(require 'init-popper)
;; 根据需要自定义 popper 的配置
```

### my-highlight-current-line
```elisp
(require 'my-highlight-current-line)
(global-my-highlight-current-line-mode 1)
```

### my-isearch-highlight-all
```elisp
(require 'my-isearch-highlight-all)
;; 在搜索时自动高亮所有匹配项
```

### gitignore-generator
```elisp
(require 'gitignore-generator)
;; M-x gitignore-generate 生成 .gitignore 文件
```

### init-wayland
```elisp
;; 在 Wayland 环境下加载
(when (getenv "WAYLAND_DISPLAY")
  (require 'init-wayland))
```

### bogster-theme
```elisp
(load-theme 'bogster t)
```

## 🔧 依赖

不同的脚本可能有不同的依赖，请根据实际情况安装：

- `popper` (init-popper.el 需要)
- `winner-mode` (enhanced-winner-mode.el 需要，Emacs 自带)

## 🤝 贡献

欢迎提交 Issue 和 Pull Request！

