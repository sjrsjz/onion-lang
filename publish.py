#!/usr/bin/env python3
"""
自动发布Onion Language Rust包到crates.io的脚本
按依赖关系正确顺序发布：onion-vm -> onion-frontend -> onion-lang
"""

import os
import sys
import subprocess
import time
from pathlib import Path
from typing import List, Dict, Optional

class CargoPublisher:
    def __init__(self, workspace_root: str):
        self.workspace_root = Path(workspace_root)
        self.packages = [
            {
                "name": "onion-vm",
                "path": self.workspace_root / "onion-vm",
                "description": "Virtual machine runtime for Onion language"
            },
            {
                "name": "onion-frontend", 
                "path": self.workspace_root / "onion-frontend",
                "description": "Compilation frontend for Onion language"
            },
            {
                "name": "onion-lang",
                "path": self.workspace_root,
                "description": "Main Onion programming language package"
            }
        ]
        
    def run_command(self, cmd: List[str], cwd: Optional[Path] = None) -> tuple[bool, str]:
        """执行命令并返回结果"""
        try:
            result = subprocess.run(
                cmd, 
                cwd=cwd or self.workspace_root,
                capture_output=True, 
                text=True,
                shell=True if os.name == 'nt' else False
            )
            return result.returncode == 0, result.stdout + result.stderr
        except Exception as e:
            return False, str(e)
    
    def check_cargo_login(self) -> bool:
        """检查是否已登录cargo"""
        print("🔍 检查cargo登录状态...")
        success, output = self.run_command(["cargo", "login", "--help"])
        if not success:
            print("❌ cargo命令不可用，请确保已安装Rust")
            return False
        
        # 尝试执行一个需要认证的命令来检查登录状态
        success, output = self.run_command(["cargo", "owner", "--list", "serde"])
        if "not currently logged in" in output.lower() or "authentication" in output.lower():
            print("❌ 未登录到crates.io，请先运行: cargo login <your-token>")
            return False
        
        print("✅ cargo已登录")
        return True
    
    def check_package_exists(self, package_name: str, version: str) -> bool:
        """检查包版本是否已存在于crates.io"""
        print(f"🔍 检查 {package_name} v{version} 是否已存在...")
        success, output = self.run_command([
            "cargo", "search", package_name, "--limit", "1"
        ])
        
        if success and f'"{package_name}"' in output and f'= "{version}"' in output:
            print(f"⚠️  {package_name} v{version} 已存在于crates.io")
            return True
        
        print(f"✅ {package_name} v{version} 可以发布")
        return False
    
    def get_package_version(self, package_path: Path) -> Optional[str]:
        """从Cargo.toml获取包版本"""
        cargo_toml = package_path / "Cargo.toml"
        if not cargo_toml.exists():
            return None
        
        try:
            with open(cargo_toml, 'r', encoding='utf-8') as f:
                content = f.read()
                for line in content.split('\n'):
                    if line.strip().startswith('version = '):
                        return line.split('"')[1]
        except Exception as e:
            print(f"❌ 读取版本失败: {e}")
        
        return None
    
    def validate_package(self, package: Dict) -> bool:
        """验证包是否可以发布"""
        print(f"\n📦 验证包: {package['name']}")
        
        package_path = package['path']
        if not package_path.exists():
            print(f"❌ 包路径不存在: {package_path}")
            return False
        
        # 检查Cargo.toml
        cargo_toml = package_path / "Cargo.toml"
        if not cargo_toml.exists():
            print(f"❌ Cargo.toml不存在: {cargo_toml}")
            return False
        
        # 获取版本
        version = self.get_package_version(package_path)
        if not version:
            print(f"❌ 无法获取版本号")
            return False
        
        print(f"📋 包信息: {package['name']} v{version}")
        
        # 检查是否已存在
        if self.check_package_exists(package['name'], version):
            return False
        
        # 运行cargo check
        print(f"🔍 检查包的编译状态...")
        success, output = self.run_command(["cargo", "check"], cwd=package_path)
        if not success:
            print(f"❌ 编译检查失败:\n{output}")
            return False
        
        print(f"✅ {package['name']} 验证通过")
        return True
    def publish_package(self, package: Dict, dry_run: bool = False, skip_existing: bool = False) -> bool:
        """发布单个包"""
        package_name = package['name']
        package_path = package['path']
        
        print(f"\n🚀 {'模拟' if dry_run else ''}发布包: {package_name}")
        print(f"📁 路径: {package_path}")
        
        # 如果启用跳过已存在包，先检查包是否已存在
        if skip_existing and not dry_run:
            version = self.get_package_version(package_path)
            if version and self.check_package_exists(package_name, version):
                print(f"⏭️  跳过已存在的包: {package_name} v{version}")
                return True
        
        # 构建发布命令
        cmd = ["cargo", "publish"]
        if dry_run:
            cmd.append("--dry-run")
        
        # 执行发布
        success, output = self.run_command(cmd, cwd=package_path)
        
        if success:
            print(f"✅ {package_name} {'模拟' if dry_run else ''}发布成功!")
            if not dry_run:
                print(f"🎉 {package_name} 已上传到 crates.io")
        else:
            # 检查是否是因为包已存在而失败
            if skip_existing and "already exists" in output:
                print(f"⏭️  包已存在，跳过: {package_name}")
                return True
            else:
                print(f"❌ {package_name} {'模拟' if dry_run else ''}发布失败:")
                print(output)
                return False
        
        return True
    def publish_all(self, dry_run: bool = False, skip_validation: bool = False, skip_existing: bool = False):
        """按顺序发布所有包"""
        print("🎯 Onion Language 包发布工具")
        print("=" * 50)
        
        if not skip_validation:
            # 检查登录状态
            if not self.check_cargo_login():
                return False
            
            # 验证所有包
            print("\n📋 验证所有包...")
            for package in self.packages:
                if not self.validate_package(package):
                    print(f"\n❌ 包验证失败，停止发布流程")
                    return False
        
        print(f"\n🚀 开始{'模拟' if dry_run else ''}发布流程...")
        print("📦 发布顺序: onion-vm -> onion-frontend -> onion-lang")
        if skip_existing:
            print("⏭️  已启用跳过已存在包模式")
        
        # 按顺序发布包
        for i, package in enumerate(self.packages):
            print(f"\n步骤 {i+1}/{len(self.packages)}")
            
            if not self.publish_package(package, dry_run, skip_existing):
                print(f"\n❌ 发布失败，停止流程")
                return False
            
            # 如果不是最后一个包且不是dry-run，等待一下让crates.io处理
            if i < len(self.packages) - 1 and not dry_run:
                print("⏳ 等待 30 秒让 crates.io 处理...")
                time.sleep(30)
        
        print(f"\n🎉 所有包{'模拟' if dry_run else ''}发布完成!")
        if not dry_run:
            print("🔗 查看你的包：")
            for package in self.packages:
                print(f"   • https://crates.io/crates/{package['name']}")
        
        return True

def main():
    import argparse
    
    parser = argparse.ArgumentParser(
        description="自动发布Onion Language包到crates.io",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
示例:
  python publish.py --dry-run    # 模拟发布，不实际上传
  python publish.py             # 正式发布到crates.io
  python publish.py --skip-validation  # 跳过验证直接发布
        """
    )
    
    parser.add_argument(
        '--dry-run', 
        action='store_true',
        help='模拟发布，不实际上传到crates.io'
    )
    parser.add_argument(
        '--skip-validation',
        action='store_true', 
        help='跳过包验证步骤'
    )
    
    parser.add_argument(
        '--skip-existing',
        action='store_true',
        help='跳过已存在的包，继续发布其他包'
    )
    
    parser.add_argument(
        '--workspace-root',
        default='.',
        help='工作空间根目录 (默认: 当前目录)'
    )
    
    args = parser.parse_args()
    
    # 创建发布器
    publisher = CargoPublisher(args.workspace_root)
    
    # 执行发布
    try:
        success = publisher.publish_all(
            dry_run=args.dry_run,
            skip_validation=args.skip_validation,
            skip_existing=args.skip_existing
        )
        sys.exit(0 if success else 1)
    except KeyboardInterrupt:
        print("\n\n⚠️  用户中断了发布流程")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ 发布过程中出现错误: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
